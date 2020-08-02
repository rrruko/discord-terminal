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
import Control.Concurrent (forkIO)
import Control.Lens
import Control.Monad (void)
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Bool
import Data.Foldable
import Data.List (elemIndex, find, sortOn)
import Data.Map.Strict ((!?))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid
import Data.Text (Text, intercalate, pack)
import qualified Data.Text as T
import Data.Witherable
import Discord
import qualified Discord.Requests as R
import Discord.Types hiding (Event)
import qualified Discord.Types
import qualified Graphics.Vty as V
import Reflex
import Reflex.Network
import Reflex.Vty
import System.Environment

import Editor (editor)
import Scrollable (ScrollableTextWindowed(..))

type DiscordEvent = Discord.Types.Event

data Lazy a
  = NotLoaded
  | Loaded a
  deriving (Eq, Ord, Show)

data AppMessage = AppMessage
  { _author :: Text
  , _contents :: Text
  , _timestamp :: UTCTime
  , _messageId' :: MessageId
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

newtype AppState = AppState
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
awaitBothEvents = awaitBothEventsWith (,)

awaitBothEventsWith
  :: (Reflex t, MonadHold t m)
  => (a -> b -> c)
  -> Event t a
  -> Event t b
  -> m (Event t c)
awaitBothEventsWith f a b = do
  aCurrent <- holdDyn Nothing (Just <$> a)
  bCurrent <- holdDyn Nothing (Just <$> b)
  let bothEvents = liftA2 (,) aCurrent bCurrent
  pure $ updated bothEvents & fmapMaybe (uncurry (liftA2 f))

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
  void (networkHold
    (text "Acquiring guilds and handle...")
    (ffor discordInit (`runAppWithHandle` discordEvent)))
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
  chans <- fmap DiscordVty._channels (_guildsMap g !? firstGId)
  minKey chans

runAppWithHandle
  :: (MonadVtyApp t m, MonadNodeId m)
  => DiscordInit
  -> Event t NewMessage
  -> VtyWidget t m ()
runAppWithHandle (DiscordInit guilds initGuildId initChanId handle) discordEvent = mdo
  (newGuildId, newChannelId, bumpTop) <-
    serverWidget
      currentGuildId
      currentChanId
      updatedAppState
      (sendUserMessage handle)
  let newChanId = updated (liftA2 (,) currentGuildId currentChanId)
  updatedAppState <-
    updateAppState
      handle
      currentGuildId
      currentChanId
      newChanId
      guilds
      discordEvent
      bumpTop
  (currentGuildId, currentChanId) <-
    accumulateGuildChannel
      initGuildId
      initChanId
      newGuildId
      newChannelId
  pure ()

sendUserMessage
  :: MonadIO m
  => DiscordHandle
  -> ChannelId
  -> Text
  -> m (Maybe RestCallErrorCode)
sendUserMessage handle cId txt = liftIO $
  restCall handle (R.CreateMessage cId txt) >>= \case
    Left errCode -> pure (Just errCode)
    Right _ -> pure Nothing

updateAppState
  :: (MonadVtyApp t m)
  => DiscordHandle
  -> Dynamic t GuildId
  -> Dynamic t ChannelId
  -> Event t (GuildId, ChannelId)
  -> AppState
  -> Event t NewMessage
  -> Event t (Maybe MessageId)
  -> VtyWidget t m (Dynamic t AppState)
updateAppState handle currGuildId currChanId newChanId guilds newMsg bumpTop = mdo
  messageUpdates <- getMessageUpdates handle newChanId updatedAppStateDyn newMsg
  oldMessageUpdates <- getOldMessages handle currGuildId currChanId bumpTop
  userUpdates <- getUserUpdates handle newChanId
  updatedAppStateDyn <- foldDyn ($) guilds
    (iterateEvent (mergeList [messageUpdates, userUpdates, oldMessageUpdates]))
  pure updatedAppStateDyn

getOldMessages
  :: (MonadVtyApp t m)
  => DiscordHandle
  -> Dynamic t GuildId
  -> Dynamic t ChannelId
  -> Event t (Maybe MessageId)
  -> VtyWidget t m (Event t (AppState -> AppState))
getOldMessages handle currGuildId currChanId req = do
  req' <- debounce 0.5 req
  let reqOldChannelMessages =
        attach (current currGuildId) (attach (current currChanId) req')
  newMessages <- performEventAsync
    (fmap
      ((\f (g,(c,m)) -> f g c m) (requestOldChannelMessages handle))
      reqOldChannelMessages)
  pure $ prependMessages <$> newMessages

requestOldChannelMessages
  :: MonadIO m
  => DiscordHandle
  -> GuildId
  -> ChannelId
  -> Maybe MessageId
  -> ((GuildId, ChannelId, [AppMessage]) -> IO ())
  -> m ()
requestOldChannelMessages handle gId cId mId callback = liftIO do
  restCall handle (R.GetChannelMessages cId (10, maybe R.LatestMessages R.BeforeMessage mId)) >>= \case
    Left _ -> pure ()
    Right msgs -> do
      let msgs' = fmap toAppMessage msgs
      callback (gId, cId, msgs')

{-
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
    Left _ -> pure ()
    Right users -> callback (gId, users)
-}

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
  pure $ iterateEvent $ fmap updateMessages <$>
    mergeList [newMessages, newCreatedMessages]

getUserUpdates
  :: (MonadVtyApp t m)
  => DiscordHandle
  -> Event t (GuildId, ChannelId)
  -> VtyWidget t m (Event t (AppState -> AppState))
getUserUpdates handle newChanId = do
  newGuildId <- uniqEvent (fmap fst newChanId)
  reqGuildUsers <- debounce 0.5 newGuildId
  users <- performEventAsync
    (fmap (requestGuildUsers handle) reqGuildUsers)
  pure (uncurry updateUsers <$> users)

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
        (\prev curr -> if prev == Just curr then Nothing else Just curr)
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
    Left _ -> pure ()
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
iterateEvent = fmap (appEndo . foldMap Endo)

updateMessages :: (GuildId, ChannelId, [AppMessage]) -> AppState -> AppState
updateMessages (gId, cId, msg) appState =
  appState & guildsMap . ix gId . channels . ix cId . messages %~ \case
    NotLoaded -> Loaded (reverse msg)
    Loaded msgs -> Loaded (msgs <> reverse msg)

prependMessages :: (GuildId, ChannelId, [AppMessage]) -> AppState -> AppState
prependMessages (gId, cId, msg) appState =
  appState & guildsMap . ix gId . channels . ix cId . messages %~ \case
    NotLoaded -> Loaded (reverse msg)
    Loaded msgs -> Loaded (reverse msg <> msgs)

lookupChannelGuild :: ChannelId -> AppState -> Maybe GuildId
lookupChannelGuild cId guilds = do
  let gs = Map.toList (_guildsMap guilds)
  fst <$> find (\(_, g) -> isJust $ Map.lookup cId (DiscordVty._channels g)) gs

requestChannelMessages
  :: MonadIO m
  => DiscordHandle
  -> GuildId
  -> ChannelId
  -> ((GuildId, ChannelId, [AppMessage]) -> IO ())
  -> m ()
requestChannelMessages handle gId cId callback = liftIO $
  restCall handle (R.GetChannelMessages cId (10, R.LatestMessages)) >>= \case
    Left _ -> pure ()
    Right msgs -> do
      let msgs' = fmap toAppMessage msgs
      callback (gId, cId, msgs')

handleDiscordEvent :: MonadIO m => DiscordEvent -> (NewMessage -> IO ()) -> m ()
handleDiscordEvent ev callback = liftIO $ do
  case ev of
    MessageCreate msg -> callback $
      NewMessage
        (toAppMessage msg)
        (messageChannel msg)
    _ -> pure ()

toAppMessage :: Message -> AppMessage
toAppMessage msg =
  AppMessage
    (userName (messageAuthor msg))
    (prettyMessageText msg)
    (messageTimestamp msg)
    (messageId msg)

prettyMessageText :: Message -> Text
prettyMessageText msg =
  let
    attachments = fmap attachmentUrl (messageAttachments msg)
    embeds = fmapMaybe embedUrl (messageEmbeds msg)
  in
    fold
      [ messageText msg
      , "\n"
      , if not (null attachments) then "Attachments:\n" else ""
      , T.unlines attachments
      , if not (null embeds) then "Embeds:\n" else ""
      , T.unlines embeds
      ]

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
    Left _ -> pure ()
    Right guilds -> do
      m <- getGuildsMap handle guilds
      callback (DiscordGuildsEvent (AppState m))

toChannelMap :: [Channel] -> Map.Map ChannelId ChannelState
toChannelMap chans =
  Map.fromList (fmap (\c -> (channelId c, initChan c)) chans)
  where
  initChan c =
    ChannelState
      (channelNamePretty c)
      (channelId c)
      (getChannelPosition c)
      (channelParentId c)
      (c & \case { ChannelGuildCategory {} -> True; _ -> False })
      NotLoaded
  getChannelPosition c = case c of
    ChannelText {} -> channelPosition c
    ChannelNews {} -> channelPosition c
    ChannelStorePage {} -> channelPosition c
    ChannelVoice {} -> channelPosition c
    ChannelGuildCategory {} -> channelPosition c
    ChannelDirectMessage {} -> Nothing
    ChannelGroupDM {} -> Nothing

getGuildsMap
  :: DiscordHandle
  -> [PartialGuild]
  -> IO (Map.Map GuildId GuildState)
getGuildsMap handle guilds = do
  chans <- forM guilds \g ->
    restCall handle (R.GetGuildChannels (partialGuildId g)) >>= \case
      Left _ -> pure (partialGuildId g, (partialGuildName g, []))
      Right chans -> pure (partialGuildId g, (partialGuildName g, chans))
  let channelMaps = fmap
        (fmap (toChannelMap . Prelude.filter isValidChannel))
        (Map.fromList chans)
  pure (fmap toGuildState channelMaps)
  where
  toGuildState (name, chans) = GuildState name chans mempty
  isValidChannel = \case
    ChannelText {} -> True
    ChannelNews {} -> True
    ChannelGuildCategory {} -> True
    _ -> False

type TriggerDiscordEvent = DiscordEvent -> IO ()
type OnStartEvent = DiscordHandle -> IO ()

spawnDiscord
  :: MonadIO m
  => Text -> TriggerDiscordEvent -> OnStartEvent -> (Text -> IO ()) -> m ()
spawnDiscord botToken triggerDiscordEvent onStartEvent callback =
  liftIO $ void $ forkIO $ do
    err <- runDiscord $ def
             { discordToken = botToken
             , discordOnEvent = const triggerDiscordEvent
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
  void (performEventAsync
    (spawnDiscord token triggerDiscordEvent onStartEvent <$ start))
  ev <- performEventAsync (fmap handleDiscordEvent discordEvent)
  startEv <- performEventAsync (fmap handleOnStartEvent discordStartEvent)
  pure (DiscordEvents ev startEv)

channelNamePretty :: Channel -> Text
channelNamePretty c = case c of
  ChannelText {} ->
    channelName c
  ChannelVoice {} ->
    channelName c
  ChannelDirectMessage {} ->
    intercalate ", " (userName <$> channelRecipients c)
  ChannelGroupDM {} ->
    intercalate ", " (userName <$> channelRecipients c)
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
  => Dynamic t GuildId
  -> Dynamic t ChannelId
  -> Dynamic t AppState
  -> (ChannelId -> Text -> Performable (VtyWidget t m) x)
  -> VtyWidget t m (Event t GuildId, Event t ChannelId, Event t (Maybe MessageId))
serverWidget currentGuildId currentChanId guilds sendMsg = do
  let currentGuild = ffor2 (fmap _guildsMap guilds) currentGuildId (Map.!)
  let chanState =
        liftA3 getChannelState
          (fmap _guildsMap guilds)
          currentGuildId
          currentChanId
  let users = guildUsers <$> currentGuild
  (newGuildId, ((userSend, bumpTop), newChanId)) <-
    serversView $ ServerViewState
      currentGuildId
      currentChanId
      currentGuild
      chanState
      users
      guilds
  let sendEv =
        sendMsg
          <$> current currentChanId
          <@> userSend
  void (performEvent sendEv)
  let oldestMsg =
        getOldestMsg <$>
          tag
            (liftA3 (,,)
              (current guilds)
              (current currentGuildId)
              (current currentChanId))
            bumpTop
  pure (newGuildId, newChanId, oldestMsg)

getOldestMsg :: (AppState, GuildId, ChannelId) -> Maybe MessageId
getOldestMsg (gs, gId, cId) =
  let msgs = gs ^? guildsMap . ix gId . channels . ix cId . messages
  in
    case msgs of
      Just (Loaded (x:_)) -> Just (_messageId' x)
      _ -> Nothing

serversView
  :: (MonadVtyApp t m, MonadNodeId m)
  => ServerViewState t
  -> VtyWidget t m (Event t GuildId, ((Event t T.Text, Event t ()), Event t ChannelId))
serversView sws = do
  inp <- key V.KEsc
  tog <- toggle False inp
  splitV (pure (const 1)) (tog <&> bool (True, False) (False, True))
    (serverList (_serverViewGId sws) (fmap _guildsMap (_serverViewGuilds sws)))
    (serverView sws)

serverView
  :: (MonadVtyApp t m, MonadNodeId m)
  => ServerViewState t
  -> VtyWidget t m ((Event t T.Text, Event t ()), Event t ChannelId)
serverView sws = do
  nav <- tabNavigation
  navTog <- toggle True nav
  let foc = fmap (bool (False, True) (True, False)) navTog
  splitH
    (pure (subtract 12))
    foc
    (boxTitle (constant def) " Channel view "
      (channelView
        (_serverViewChannel sws)
        (_serverViewUsers sws)))
    (boxTitle (constant def) " Channels "
      (channelList
        (_serverViewCId sws)
        (fmap DiscordVty._channels
          (_serverViewGuild sws))))

guildUsers :: GuildState -> Map.Map Text UserId
guildUsers = Map.foldrWithKey nameToId mempty . _members
  where
  nameToId _ member =
    let user = memberUser member
    in  Map.insert (userName user) (userId user)

accumulateGuildChannel
  :: (Reflex t, MonadHold t m, MonadFix m)
  => GuildId
  -> ChannelId
  -> Event t GuildId
  -> Event t ChannelId
  -> m (Dynamic t GuildId, Dynamic t ChannelId)
accumulateGuildChannel initGuildId initChanId newGuildId newChanId = do
  currentGuildId <- foldDyn const initGuildId newGuildId
  selectedChannels <-
    foldDyn
      (uncurry Map.insert)
      mempty
      (attachPromptlyDyn currentGuildId newChanId)
  let savedChanId = ffor2 selectedChannels currentGuildId (!?)
  updatedChanId <- foldDyn const initChanId newChanId
  let currentChanId = ffor2 updatedChanId savedChanId fromMaybe
  pure (currentGuildId, currentChanId)

getChannelState
  :: Map.Map GuildId GuildState
  -> GuildId
  -> ChannelId
  -> Maybe ChannelState
getChannelState s gId cId = do
  currGuild <- s !? gId
  DiscordVty._channels currGuild !? cId

highlight :: Reflex t => Dynamic t Bool -> RichTextConfig t
highlight isFocused =
  RichTextConfig (current isFocused <&> bool normal highlighted)
  where
    normal = V.defAttr
    highlighted = V.withStyle V.defAttr V.standout

serverList
  :: forall t m. (MonadVtyApp t m, MonadNodeId m)
  => Dynamic t GuildId
  -> Dynamic t (Map.Map GuildId GuildState)
  -> VtyWidget t m (Event t GuildId)
serverList selected guilds = do
  e <- networkView $
    ffor2 selected guilds \s g ->
      optionList s g Orientation_Row const \gId guild ->
        stretch do
          f <- focus
          richText (highlight f) (pure (_guildName' guild))
          pure (bool (First Nothing) (First $ Just gId) <$> f)
  switchHold never e

channelList
  :: forall t m. (MonadVtyApp t m, MonadNodeId m)
  => Dynamic t ChannelId
  -> Dynamic t (Map.Map ChannelId ChannelState)
  -> VtyWidget t m (Event t ChannelId)
channelList selected chans = do
  e <- networkView $
    ffor2 selected chans \s c ->
      optionList s c Orientation_Column (channelKey c) \cId channel ->
        fixed 1 do
          f <- focus
          richText (highlight f) (pure (_channelName' channel))
          pure (bool (First Nothing) (First $ Just cId) <$> f)
  switchHold never e
  where
    channelKey cs _ v = channelSortKey (Map.elems cs) v

channelSortKey :: [ChannelState] -> ChannelState -> (Maybe Integer, Maybe Integer)
channelSortKey chans this =
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
    (fromMaybe 0 selIndex)
    (leftmost
      [ (-1) <$ up
      , 1 <$ down
      ])
    (makeList m)
  where
  makeList m' =
    fmap
      (updated . fmap getFirst . mconcat)
      (forM (sortOn (uncurry sortKey) (Map.toList m')) (uncurry pretty))

channelView
  :: forall t m. (Reflex t, MonadHold t m, MonadFix m, MonadNodeId m, NotReady t m, Adjustable t m, PostBuild t m)
  => Dynamic t (Maybe ChannelState)
  -> Dynamic t (Map.Map Text UserId)
  -> VtyWidget t m (Event t Text, Event t ())
channelView chanState users = mdo
  (windowedText, userSend) <- splitV
    (pure (subtract 2))
    (pure (False, True))
    (scrollableWidgets
      never
      (csToWidgets <$> chanState))
    (editor users)
  pure (userSend, scrollableTextWindowed_bumpTop windowedText)
  where
    csToWidgets :: Maybe ChannelState -> [VtyWidget t m ()]
    csToWidgets (Just m) =
      case _messages m of
        Loaded m' -> fmap prettyMessage m'
        NotLoaded -> [text "Not loaded"]
    csToWidgets Nothing = [text "Failed to get channel state"]
    prettyMessage :: AppMessage -> VtyWidget t m ()
    prettyMessage msg = text $ pure $
      T.pack (show (_timestamp msg)) <>
      "\n" <>
      _author msg <>
      ": " <>
      _contents msg <>
      "\n"

scrollableWidgets
  :: (Reflex t, MonadHold t m, MonadFix m, MonadNodeId m, NotReady t m, Adjustable t m, PostBuild t m)
  => Event t Int
  -> Dynamic t [VtyWidget t m a]
  -> VtyWidget t m (ScrollableTextWindowed t)
scrollableWidgets _ widgets =
  pure undefined
