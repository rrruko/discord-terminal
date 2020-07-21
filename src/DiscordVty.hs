{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}

module DiscordVty
  ( app
  , channelView
  ) where

import Control.Applicative
import Control.Concurrent (forkIO, threadDelay)
import Control.Lens
import Control.Lens.Operators
import Control.Monad (when, void)
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Bifunctor
import Data.Bitraversable
import Data.Bool
import Data.Foldable
import Data.Function (on)
import Data.List (elemIndex, find, sortOn, partition)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid
import qualified Data.Set as Set
import Data.Text (Text, intercalate, isPrefixOf, unlines, pack, toLower)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Zipper
import Data.Witherable
import Discord
import qualified Discord.Requests as R
import Discord.Types hiding (Event)
import qualified Discord.Types
import qualified Graphics.Vty as V
import Reflex
import Reflex.Pure
import Reflex.Network
import Reflex.Vty
import Reflex.Vty.Widget
import System.Environment

import Editor (editor)
import Scrollable (scrollableTextWindowed)

type DiscordEvent = Discord.Types.Event

data Lazy a
  = NotLoaded
  | Loaded a
  deriving (Eq, Ord, Show)

data AppMessage = AppMessage
  { _author :: Text
  , _contents :: Text
  , _timestamp :: UTCTime
  } deriving (Show)
makeLenses ''AppMessage

data ChannelState = ChannelState
  { _channelName' :: Text
  , _channelId' :: ChannelId
  , _channelPosition' :: Maybe Integer
  , _channelParentId' :: Maybe ChannelId
  , _channelIsCategory :: Bool
  , _messages :: Lazy [AppMessage]
  } deriving (Show)
makeLenses ''ChannelState

data GuildState = GuildState
  { _guildName' :: Text
  , _channels :: Map.Map ChannelId ChannelState
  , _members :: Map.Map UserId GuildMember
  }
makeLenses ''GuildState

data AppState = AppState
  { _guildsMap :: Map.Map GuildId GuildState
  }
makeLenses ''AppState

app :: IO ()
app =
  getArgs >>= \case
    (botToken : _) -> runClient (pack botToken)
    _ -> putStrLn "No token supplied."

awaitBothEvents
  :: (Reflex t, MonadHold t m)
  => Event t a
  -> Event t b
  -> m (Event t (a, b))
awaitBothEvents a b = do
  aCurrent <- holdDyn Nothing (Just <$> a)
  bCurrent <- holdDyn Nothing (Just <$> b)
  let both = liftA2 (,) aCurrent bCurrent
  pure $ updated both & fmapMaybe (uncurry (liftA2 (,)))

awaitBothEventsWith
  :: (Reflex t, MonadHold t m)
  => (a -> b -> c)
  -> Event t a
  -> Event t b
  -> m (Event t c)
awaitBothEventsWith f a b = do
  aCurrent <- holdDyn Nothing (Just <$> a)
  bCurrent <- holdDyn Nothing (Just <$> b)
  let both = liftA2 (,) aCurrent bCurrent
  pure $ updated both & fmapMaybe (uncurry (liftA2 f))

data DiscordInit = DiscordInit
  { _initGuilds :: AppState
  , _initGuildId :: GuildId
  , _initChannelId :: ChannelId
  , _initHandle :: DiscordHandle
  }

runClient :: Text -> IO ()
runClient token = mainWidget mdo
  DiscordEvents discordEvent discordStartEvent <- setupDiscord token
  let guilds = discordStartEvent
        & fmapMaybe getGuildsEvent
        & fmapMaybe (\g -> liftA3 (,,) (Just g) (firstGuild g) (firstChannel g))
  let handle = discordStartEvent
        & fmapMaybe getHandleEvent
  discordInit <-
    awaitBothEventsWith
      (\(a,b,c) d -> DiscordInit a b c d)
      guilds
      handle
  networkHold
    (text "Acquiring guilds and handle...")
    (ffor discordInit (\gh -> runAppWithHandle gh discordEvent))
  inp <- input
  pure $ fforMaybe inp $ \case
    V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
    _ -> Nothing

minKey :: Map.Map a b -> Maybe a
minKey = fmap (fst . fst) . Map.minViewWithKey

firstGuild :: AppState -> Maybe GuildId
firstGuild g = minKey (_guildsMap g)

firstChannel :: AppState -> Maybe ChannelId
firstChannel g = do
  firstGId <- firstGuild g
  let firstGuild = (_guildsMap g) Map.!? firstGId
  chans <- fmap DiscordVty._channels firstGuild
  minKey chans

runAppWithHandle
  :: (MonadVtyApp t m, MonadNodeId m)
  => DiscordInit
  -> Event t NewMessage
  -> VtyWidget t m ()
runAppWithHandle (DiscordInit guilds initGuildId initChanId handle) discordEvent = mdo
  (newGuildId, newChannelId) <-
    serverWidget
      handle
      currentGuildId
      currentChanId
      updatedAppState
      sendUserMessage
  let newChanId = updated (liftA2 (,) currentGuildId currentChanId)
  updatedAppState <- updateAppState handle newChanId guilds discordEvent
  (currentGuildId, currentChanId) <-
    accumulateGuildChannel
      initGuildId
      initChanId
      newGuildId
      newChannelId
      updatedAppState
  pure ()

sendUserMessage :: MonadIO m => (DiscordHandle, Text, ChannelId) -> m (Maybe RestCallErrorCode)
sendUserMessage (handle, text, cId) = liftIO $
  restCall handle (R.CreateMessage cId text) >>= \case
    Left errCode -> pure (Just errCode)
    Right _ -> pure Nothing

updateAppState
  :: (MonadVtyApp t m)
  => DiscordHandle
  -> Event t (GuildId, ChannelId)
  -> AppState
  -> Event t NewMessage
  -> VtyWidget t m (Dynamic t AppState)
updateAppState handle newChanId guilds newMsg = mdo
  messageUpdates <- getMessageUpdates handle newChanId updatedAppStateDyn newMsg
  userUpdates <- getUserUpdates handle newChanId updatedAppStateDyn
  updatedAppStateDyn <- foldDyn ($) guilds
    (iterateEvent (mergeList [messageUpdates, userUpdates]))
  pure updatedAppStateDyn

getMessageUpdates
  :: (MonadVtyApp t m)
  => DiscordHandle
  -> Event t (GuildId, ChannelId)
  -> Dynamic t AppState
  -> Event t NewMessage
  -> VtyWidget t m (Event t (AppState -> AppState))
getMessageUpdates handle newChanId updatedAppStateDyn newMsg = do
  newChanId' <- debounce 0.5 newChanId
  let reqChannelMessages = requestChannelMessageUpdate newChanId' updatedAppStateDyn
  let newCreatedMessages = getNewMessageContext (current updatedAppStateDyn) newMsg
  newMessages <- performEventAsync
    (fmap (uncurry (requestChannelMessages handle)) reqChannelMessages)
  pure $ iterateEvent $ fmap (fmap updateMessages) $
    (mergeList [newMessages, newCreatedMessages])

getUserUpdates
  :: (MonadVtyApp t m)
  => DiscordHandle
  -> Event t (GuildId, ChannelId)
  -> Dynamic t AppState
  -> VtyWidget t m (Event t (AppState -> AppState))
getUserUpdates handle newChanId updatedAppStateDyn = do
  newGuildId <- uniqEvent (fmap (\(a,_) -> a) newChanId)
  reqGuildUsers <- debounce 0.5 newGuildId
  guildUsers <-
    (performEventAsync
      (fmap (requestGuildUsers handle) reqGuildUsers))
  pure (uncurry updateUsers <$> guildUsers)

requestChannelMessageUpdate
  :: (Reflex t)
  => Event t (GuildId, ChannelId)
  -> Dynamic t AppState
  -> Event t (GuildId, ChannelId)
requestChannelMessageUpdate requestedChanId appState =
  attachPromptlyDyn appState requestedChanId
    & fmapMaybe (\(gs, (gId, cId)) ->
          if loaded gs gId cId
            then Nothing
            else Just (gId, cId))

loaded :: AppState -> GuildId -> ChannelId -> Bool
loaded gs gId cId =
  case msgs of
    Just NotLoaded -> False
    _ -> True
  where
    msgs = gs ^? guildsMap . ix gId . channels . ix cId . messages

uniqEvent :: (Reflex t, MonadHold t m, Eq a) => Event t a -> m (Event t a)
uniqEvent ev = do
  latest <- holdDyn Nothing (fmap Just ev)
  let fireWhenNew = attachWith
        (\last curr -> if last == Just curr then Nothing else Just curr)
        (current latest)
        ev
  pure (catMaybes fireWhenNew)

updateUsers :: GuildId -> [GuildMember] -> AppState -> AppState
updateUsers gId newMembers appState =
  appState & guildsMap . ix gId . members %~
    const (Map.fromList (fmap (\x -> (userId (memberUser x), x)) newMembers))

requestGuildUsers
  :: MonadIO m
  => DiscordHandle
  -> GuildId
  -> ((GuildId, [GuildMember]) -> IO ())
  -> m ()
requestGuildUsers handle gId callback = liftIO do
  restCall handle (R.ListGuildMembers gId (R.GuildMembersTiming (Just 100) Nothing)) >>= \case
    Left errCode -> pure ()
    Right users -> callback (gId, users)

getNewMessageContext
  :: (Reflex t)
  => Behavior t AppState
  -> Event t NewMessage
  -> Event t (GuildId, ChannelId, [AppMessage])
getNewMessageContext guilds newMsg = do
  let e = attach guilds newMsg
  catMaybes $ ffor e (\(gs, m) -> do
    gId <- lookupChannelGuild (newMessageChannelId m) gs
    pure (gId, newMessageChannelId m, [newMessageContent m]))

iterateEvent
  :: (Reflex t, Foldable f, Functor f)
  => Event t (f (a -> a))
  -> Event t (a -> a)
iterateEvent ev = fmap (appEndo . fold . fmap Endo) ev

updateMessages :: (GuildId, ChannelId, [AppMessage]) -> AppState -> AppState
updateMessages (gId, cId, msg) appState =
  appState & guildsMap . ix gId . channels . ix cId . messages %~ \case
    NotLoaded -> Loaded (reverse msg)
    Loaded msgs -> Loaded (msgs <> msg)

lookupChannelGuild :: ChannelId -> AppState -> Maybe GuildId
lookupChannelGuild cId guilds = do
  let gs = Map.toList (_guildsMap guilds)
  fst <$> find (\(_, gs) -> isJust $ Map.lookup cId (DiscordVty._channels gs)) gs

requestChannelMessages
  :: MonadIO m
  => DiscordHandle
  -> GuildId
  -> ChannelId
  -> ((GuildId, ChannelId, [AppMessage]) -> IO ())
  -> m ()
requestChannelMessages handle gId cId callback = liftIO $
  restCall handle (R.GetChannelMessages cId (10, R.LatestMessages)) >>= \case
    Left errCode -> pure ()
    Right msgs -> do
      let toAppMessage m = AppMessage
            (userName (messageAuthor m))
            (messageText m)
            (messageTimestamp m)
      let msgs' = fmap toAppMessage msgs
      callback (gId, cId, msgs')

handleDiscordEvent
  :: MonadIO m
  => DiscordHandle -> DiscordEvent -> (NewMessage -> IO ()) -> m ()
handleDiscordEvent handle ev callback = liftIO $ do
  case ev of
    MessageCreate msg -> callback $
      NewMessage
        (AppMessage (userName $ messageAuthor msg) (messageText msg) (messageTimestamp msg))
        (messageChannel msg)
    _ -> pure ()

data NewMessage = NewMessage
  { newMessageContent :: AppMessage
  , newMessageChannelId :: ChannelId
  }

handleOnStartEvent
  :: MonadIO m
  => DiscordHandle -> (DiscordStartEvent -> IO ()) -> m ()
handleOnStartEvent handle callback = liftIO $ do
  callback (DiscordHandleEvent handle)
  restCall handle R.GetCurrentUserGuilds >>= \case
    Left errCode -> pure ()
    Right guilds -> do
      m <- getGuildsMap handle guilds
      callback (DiscordGuildsEvent (AppState m))

toChannelMap :: [Channel] -> Map.Map ChannelId ChannelState
toChannelMap chans = Map.fromList [ (channelId c, initChan c) | c <- chans ]
  where
  initChan c =
    ChannelState
      (channelNamePretty c)
      (channelId c)
      (channelPosition' c)
      (channelParentId c)
      (c & \case { ChannelGuildCategory {} -> True; _ -> False })
      NotLoaded
  channelPosition' c = case c of
    ChannelText {} -> channelPosition c
    ChannelNews {} -> channelPosition c
    ChannelStorePage {} -> channelPosition c
    ChannelVoice {} -> channelPosition c
    ChannelGuildCategory {} -> channelPosition c
    ChannelDirectMessage {} -> Nothing
    ChannelGroupDM {} -> Nothing

getGuildsMap :: DiscordHandle -> [PartialGuild] -> IO (Map.Map GuildId GuildState)
getGuildsMap handle guilds = do
  chans <- forM guilds \g ->
    restCall handle (R.GetGuildChannels (partialGuildId g)) >>= \case
      Left errCode -> pure (partialGuildId g, [])
      Right chans -> pure (partialGuildId g, chans)
  let
    channelMap = fmap toChannelMap (Map.fromList chans)
  pure
    (Map.fromList
      [ (partialGuildId g, GuildState (partialGuildName g) (channelMap Map.! partialGuildId g) mempty)
      | g <- guilds ])
  where
  isValidChannel = \case
    ChannelText {} -> True
    ChannelNews {} -> True
    ChannelGuildCategory {} -> True
    _ -> False

type TriggerDiscordEvent = (DiscordHandle, DiscordEvent) -> IO ()
type OnStartEvent = DiscordHandle -> IO ()

spawnDiscord
  :: MonadIO m
  => Text -> TriggerDiscordEvent -> OnStartEvent -> (Text -> IO ()) -> m ()
spawnDiscord botToken triggerDiscordEvent onStartEvent callback =
  liftIO $ void $ forkIO $ do
    err <- runDiscord $ def
             { discordToken = botToken
             , discordOnEvent = curry triggerDiscordEvent
             , discordOnStart = onStartEvent
             }
    callback err

data DiscordStartEvent
  = DiscordHandleEvent DiscordHandle
  | DiscordGuildsEvent AppState

getHandleEvent :: DiscordStartEvent -> Maybe DiscordHandle
getHandleEvent = \case
  DiscordHandleEvent h -> Just h
  _ -> Nothing

getGuildsEvent :: DiscordStartEvent -> Maybe AppState
getGuildsEvent = \case
  DiscordGuildsEvent g -> Just g
  _ -> Nothing

data DiscordEvents t = DiscordEvents
  { event :: Event t NewMessage
  , startEvent :: Event t DiscordStartEvent
  }

setupDiscord
  :: (MonadVtyApp t m, MonadNodeId m)
  => Text
  -> VtyWidget t m (DiscordEvents t)
setupDiscord token = do
  (discordEvent, triggerDiscordEvent) <- newTriggerEvent
  (discordStartEvent, onStartEvent) <- newTriggerEvent
  start <- getPostBuild
  performEventAsync
    (spawnDiscord token triggerDiscordEvent onStartEvent <$ start)
  ev <- performEventAsync (fmap (uncurry handleDiscordEvent) discordEvent)
  startEv <- performEventAsync (fmap handleOnStartEvent discordStartEvent)
  pure (DiscordEvents ev startEv)

channelNamePretty :: Channel -> Text
channelNamePretty c = case c of
  ChannelText {} ->
    channelName c
  ChannelVoice {} ->
    channelName c
  ChannelDirectMessage {} ->
    intercalate ", " (fmap userName $ channelRecipients c)
  ChannelGroupDM {} ->
    intercalate ", " (fmap userName $ channelRecipients c)
  ChannelGuildCategory {} ->
    "[" <> channelName c <> "]"
  _ ->
    "Unknown"

data ServerViewState t = ServerViewState
  { _serverViewGId :: Dynamic t GuildId
  , _serverViewCId :: Dynamic t ChannelId
  , _serverViewGuild :: Dynamic t GuildState
  , _serverViewChannel :: Dynamic t (Maybe ChannelState)
  , _serverViewUsers :: Dynamic t (Map.Map Text UserId)
  , _serverViewGuilds :: Dynamic t AppState
  }

serverWidget
  :: (MonadVtyApp t m, MonadNodeId m)
  => DiscordHandle
  -> Dynamic t GuildId
  -> Dynamic t ChannelId
  -> Dynamic t AppState
  -> ((DiscordHandle, Text, ChannelId) -> ReaderT (VtyWidgetCtx t) (Performable m) (Maybe RestCallErrorCode))
  -> VtyWidget t m (Event t GuildId, Event t ChannelId)
serverWidget handle currentGuildId currentChanId guilds sendUserMessage = mdo
  (newGuildId, (userSend, newChanId)) <-
    serversView $ ServerViewState
      currentGuildId
      currentChanId
      currentGuild
      chanState
      users
      guilds
  let users = guildUsers currentGuild
  let currentGuild =
        liftA2 (\g gId -> (_guildsMap g) Map.! gId) guilds currentGuildId
  let chanState =
        liftA3 getChannelState
          (fmap _guildsMap guilds)
          currentGuildId
          currentChanId
  let sendEv = attach (pure handle) (attach (current currentChanId) userSend)
  performEvent (sendUserMessage
    <$> (sendEv & fmap (\(a,(b,c)) -> (a,c,b))))
  pure (newGuildId, newChanId)

serversView
  :: (MonadVtyApp t m, MonadNodeId m)
  => ServerViewState t
  -> VtyWidget t m (Event t GuildId, (Event t T.Text, Event t ChannelId))
serversView sws = do
  inp <- key V.KEsc
  tog <- toggle False inp
  splitV (pure (const 1)) (tog <&> bool (True, False) (False, True))
    (serverList (_serverViewGId sws) (fmap _guildsMap (_serverViewGuilds sws)))
    (serverView sws)

serverView
  :: (MonadVtyApp t m, MonadNodeId m)
  => ServerViewState t
  -> VtyWidget t m (Event t T.Text, Event t ChannelId)
serverView sws = do
  nav <- tabNavigation
  navTog <- toggle True nav
  let foc = fmap (bool (False, True) (True, False)) navTog
  (splitH
    (pure (subtract 12))
    foc
    (boxTitle (constant def) " Channel view "
      (channelView
        (_serverViewChannel sws)
        (_serverViewUsers sws)))
    (boxTitle (constant def) " Channels "
      (channelsView
        (_serverViewCId sws)
        (fmap DiscordVty._channels
          (_serverViewGuild sws)))))

guildUsers :: Reflex t => Dynamic t GuildState -> Dynamic t (Map.Map Text UserId)
guildUsers =
  fmap
    (Map.map (\mem -> userId (memberUser mem))
      . (\uToM -> Map.mapKeys (\uId -> userName (memberUser (uToM Map.! uId))) uToM)
      . _members)

accumulateGuildChannel
  :: (Reflex t, MonadHold t m, MonadFix m)
  => GuildId
  -> ChannelId
  -> Event t GuildId
  -> Event t ChannelId
  -> Dynamic t AppState
  -> m (Dynamic t GuildId, Dynamic t ChannelId)
accumulateGuildChannel initGuildId initChanId newGuildId newChanId guilds = do
  currentGuildId <- foldDyn const initGuildId newGuildId
  selectedChannels <-
    foldDyn
      (uncurry Map.insert)
      mempty
      (attachPromptlyDyn currentGuildId newChanId)
  let savedChanId = ffor2 selectedChannels currentGuildId (Map.!?)
  updatedChanId <- foldDyn const initChanId newChanId
  let currentChanId = ffor2 updatedChanId savedChanId fromMaybe
  pure (currentGuildId, currentChanId)

elemAt' :: Int -> Map.Map a b -> Maybe (a, b)
elemAt' n m =
  let
    rest = snd (Map.splitAt n m)
  in
    if length rest > 0
      then Just (Map.elemAt 0 rest)
      else Nothing

getChannelState
  :: Map.Map GuildId GuildState
  -> GuildId
  -> ChannelId
  -> Maybe ChannelState
getChannelState s gId cId = do
  currGuild <- s Map.!? gId
  DiscordVty._channels currGuild Map.!? cId

normal = V.defAttr
highlighted = V.withStyle V.defAttr V.standout
highlight isFocused =
  RichTextConfig (current isFocused <&> bool normal highlighted)

serverList
  :: forall t m. (MonadVtyApp t m, MonadNodeId m)
  => Dynamic t GuildId
  -> Dynamic t (Map.Map GuildId GuildState)
  -> VtyWidget t m (Event t GuildId)
serverList selected guilds = join $ fmap (switchHold never) $ networkView $
  ffor2 selected guilds \s g ->
    optionList s g Orientation_Row const \gId guild ->
      stretch do
        f <- focus
        richText (highlight f) (pure (_guildName' guild))
        pure (bool (First Nothing) (First $ Just gId) <$> f)

channelsView
  :: forall t m. (MonadVtyApp t m, MonadNodeId m)
  => Dynamic t ChannelId
  -> Dynamic t (Map.Map ChannelId ChannelState)
  -> VtyWidget t m (Event t ChannelId)
channelsView selected channels = join $ fmap (switchHold never) $ networkView $
  ffor2 selected channels \s c ->
    optionList s c Orientation_Column (\k v -> sortKey (Map.elems c) v) \cId channel ->
      fixed 1 do
        f <- focus
        richText (highlight f) (pure (_channelName' channel))
        pure (bool (First Nothing) (First $ Just cId) <$> f)

sortKey :: [ChannelState] -> ChannelState -> (Maybe Integer, Maybe Integer)
sortKey chans this =
  let
    parentKey = _channelParentId' this
    parentChannel = find ((==parentKey) . Just . _channelId') chans
    parentPosition = _channelPosition' =<< parentChannel
    textChannelPosition
      | _channelIsCategory this = Nothing
      | otherwise = _channelPosition' this
  in
    ( getFirst (foldMap First [parentPosition, _channelPosition' this])
    , textChannelPosition
    )

optionList
  :: forall t m k v a. (MonadVtyApp t m, MonadNodeId m, Ord k, Ord a)
  => k
  -> Map.Map k v
  -> Orientation
  -> (k -> v -> a)
  -> (k -> v -> Layout t m (Dynamic t (First k)))
  -> VtyWidget t m (Event t k)
optionList selected m orientation sortKey pretty = do
  up <- keys [V.KUp, V.KChar 'k']
  down <- keys [V.KDown, V.KChar 'j']
  let selIndex = elemIndex selected (fmap fst (sortOn (uncurry sortKey) (Map.toList m)))
  catMaybes <$> runLayout
    (constDyn orientation)
    (maybe 0 id selIndex)
    (leftmost
      [ (-1) <$ up
      , 1 <$ down
      ])
    (makeList m)
  where
  makeList m' =
    fmap
      (updated . fmap getFirst . mconcat)
      (forM ((sortOn (uncurry sortKey) (Map.toList m'))) (uncurry pretty))

channelView
  :: (Reflex t, MonadHold t m, MonadFix m, MonadNodeId m, NotReady t m, Adjustable t m, PostBuild t m)
  => Dynamic t (Maybe ChannelState)
  -> Dynamic t (Map.Map Text UserId)
  -> VtyWidget t m (Event t Text)
channelView chanState users = mdo
  (progressB, userSend) <- splitV
    (pure (subtract 2))
    (pure (False, True))
    (scrollableTextWindowed
      never
      (csToLines <$> chanState))
    (editor users)
  pure userSend
  where
    csToLines :: Maybe ChannelState -> [Text]
    csToLines (Just m) =
      case _messages m of
        Loaded m' -> fmap prettyMessage m'
        NotLoaded -> ["Not loaded"]
    csToLines Nothing = ["Failed to get channel state"]
    prettyMessage :: AppMessage -> Text
    prettyMessage msg =
      T.pack (show (_timestamp msg)) <>
      "\n" <>
      _author msg <>
      ": " <>
      _contents msg <>
      "\n"
