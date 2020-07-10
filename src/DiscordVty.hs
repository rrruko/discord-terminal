{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}

module DiscordVty
  ( app
  , channelView
  , scrollableTextWindowed
  ) where

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
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import qualified Data.Set as Set
import Data.Text (Text, intercalate, isPrefixOf, unlines, pack, toLower)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Zipper
import Discord
import qualified Discord.Requests as R
import Discord.Types
import qualified Graphics.Vty as V
import Reflex
import Reflex.Pure
import Reflex.Network
import Reflex.Vty
import Reflex.Vty.Widget
import System.Environment

type ReflexEvent = Reflex.Vty.Event
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
  , _messages :: Lazy [AppMessage]
  } deriving (Show)
makeLenses ''ChannelState

data GuildState = GuildState
  { _guildName' :: Text
  , _channels :: Map.Map ChannelId ChannelState
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

runClient :: Text -> IO ()
runClient token = mainWidget mdo
  DiscordEvents discordEvent discordStartEvent <- setupDiscord token
  guilds <- holdDyn (AppState mempty)
    (fmapMaybe getGuildsEvent discordStartEvent)
  handle <- improvingMaybe =<< holdDyn Nothing
    (fmapMaybe (Just . getHandleEvent) discordStartEvent)
  tog <- toggle True =<< tabNavigation
  let foc = fmap (bool (False, True) (True, False)) tog
  currChanId <- serverWidget handle foc updatedAppState sendUserMessage
  updatedAppState <- updateAppState handle currChanId guilds discordEvent
  inp <- input
  pure $ fforMaybe inp $ \case
    V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
    _ -> Nothing

sendUserMessage :: MonadIO m => (DiscordHandle, Text, ChannelId) -> m (Maybe RestCallErrorCode)
sendUserMessage (handle, text, cId) = liftIO $
  restCall handle (R.CreateMessage cId text) >>= \case
    Left errCode -> pure (Just errCode)
    Right _ -> pure Nothing

updateAppState
  :: (MonadVtyApp t m)
  => Dynamic t (Maybe DiscordHandle)
  -> ReflexEvent t (GuildId, ChannelId, Bool)
  -> Dynamic t AppState
  -> ReflexEvent t NewMessage
  -> VtyWidget t m (Dynamic t AppState)
updateAppState handle currChanId guilds newMsg = do
  reqChannelMessages <- debounce 0.5 currChanId
    <&> fmapMaybe (\(a,b,c) -> guard (not c) *> Just (a,b))
  let handleAvailable = fmapMaybe id (updated handle)
      newCreatedMessages = getNewMessageContext (current guilds) newMsg
  newMessages <- switchDyn <$> networkHold
    (pure never)
    ((\h -> performEventAsync (fmap (uncurry (requestChannelMessages h)) reqChannelMessages))
      <$> handleAvailable)
  let appStateUpdates = iterateEvent $ fmap (fmap updateMessages) $
        (mergeList [newMessages, newCreatedMessages])
  updatedAppStateDyn <- foldDyn (.) id appStateUpdates
  pure (updatedAppStateDyn <*> guilds)

getNewMessageContext
  :: (Reflex t)
  => Behavior t AppState
  -> ReflexEvent t NewMessage
  -> ReflexEvent t (GuildId, ChannelId, [AppMessage])
getNewMessageContext guilds newMsg = do
  let e = attach guilds newMsg
  fmapMaybe id $ ffor e (\(gs, m) -> do
    gId <- lookupChannelGuild (newMessageChannelId m) gs
    pure (gId, newMessageChannelId m, [newMessageContent m]))

iterateEvent
  :: (Reflex t, Foldable f, Functor f)
  => ReflexEvent t (f (a -> a))
  -> ReflexEvent t (a -> a)
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
toChannelMap chans = Map.fromList
  [ (channelId c, ChannelState (channelNamePretty c) NotLoaded) | c <- chans ]

getGuildsMap :: DiscordHandle -> [PartialGuild] -> IO (Map.Map GuildId GuildState)
getGuildsMap handle guilds = do
  chans <- forM guilds \g ->
    restCall handle (R.GetGuildChannels (partialGuildId g)) >>= \case
      Left errCode -> pure (partialGuildId g, [])
      Right chans -> pure (partialGuildId g, chans)
  let channelMap = fmap toChannelMap (Map.fromList chans)
  pure
    (Map.fromList
      [ (partialGuildId g, GuildState (partialGuildName g) (channelMap Map.! partialGuildId g))
      | g <- guilds ])

type TriggerDiscordEvent = (DiscordHandle, Discord.Types.Event) -> IO ()
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
  { event :: ReflexEvent t NewMessage
  , startEvent :: ReflexEvent t DiscordStartEvent
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
    "<Category>"
  _ ->
    "Unknown"

serverWidget
  :: (MonadVtyApp t m, MonadNodeId m)
  => Dynamic t (Maybe DiscordHandle)
  -> Dynamic t (Bool, Bool)
  -> Dynamic t AppState
  -> ((DiscordHandle, Text, ChannelId) -> ReaderT (VtyWidgetCtx t) (Performable m) (Maybe RestCallErrorCode))
  -> VtyWidget t m (ReflexEvent t (GuildId, ChannelId, Bool))
serverWidget handle foc guilds sendUserMessage = mdo
  inp <- key V.KEsc
  tog <- toggle False inp
  (newGuildId, (userSend, newChanId)) <-
    splitV (pure (const 1)) (tog <&> bool (True, False) (False, True))
      (serversView currentGuildId (fmap _guildsMap guilds))
      (splitH
          (pure (subtract 12))
          foc
          (boxTitle (constant def) " Channel view "
            (channelView chanState))
          (boxTitle (constant def) " Channels "
            (channelsView currentChanId (fmap (fmap DiscordVty._channels) currentGuild))))

  (currentGuildId, currentChanId) <- accumulateGuildChannel
    currentGuild
    newGuildId
    newChanId
    guilds
  let currentGuild = (\g gId -> gId >>= \i -> (_guildsMap g) Map.!? i) <$> guilds <*> currentGuildId
  let chanState = (\s gId cId -> getChannelState s gId cId)
        <$> (fmap _guildsMap guilds)
        <*> currentGuildId
        <*> currentChanId
  let sendEv = attach (current handle) (attach (current currentChanId) userSend)
  performEvent (sendUserMessage
    <$> (sendEv & fmapMaybe (\(a,(b,c)) -> (,,) <$> a <*> pure c <*> b)))
  let isLoaded = fmap
        (\case { Just (ChannelState _ NotLoaded) -> False; _ -> True })
        chanState
  let updatedGuildChanId = (,,) <$> currentGuildId <*> currentChanId <*> isLoaded
  pure (fforMaybe (updated updatedGuildChanId) (\(a,b,c) -> (,,) <$> a <*> b <*> Just c))

accumulateGuildChannel
  :: (Reflex t, MonadHold t m, MonadFix m)
  => Dynamic t (Maybe GuildState)
  -> ReflexEvent t GuildId
  -> ReflexEvent t ChannelId
  -> Dynamic t AppState
  -> m (Dynamic t (Maybe GuildId), Dynamic t (Maybe ChannelId))
accumulateGuildChannel currentGuild newGuildId newChanId guilds = do
  let initGuildId = guilds <&> (fmap fst . elemAt' 0 . _guildsMap)
  updatedGuildId <- foldDyn (\x acc -> Just x) Nothing newGuildId
  let currentGuildId = getFirst <$> mconcat
        (fmap (fmap First) [ updatedGuildId, initGuildId ])
  initChanId <- holdUniqDyn (currentGuild <&> (fmap fst . (>>= elemAt' 0 . DiscordVty._channels)))

  updatedChanId <- foldDyn (\x acc -> Just x) Nothing newChanId
  currentChanId <- fmap getFirst <$> (holdDyn (First Nothing) $
    fmap First $ (leftmost [updated updatedChanId, updated initChanId]))
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
  -> Maybe GuildId
  -> Maybe ChannelId
  -> Maybe ChannelState
getChannelState s gId cId = do
  g <- gId
  c <- cId
  currGuild <- s Map.!? g
  DiscordVty._channels currGuild Map.!? c

normal = V.defAttr
highlighted = V.withStyle V.defAttr V.standout
highlight isFocused =
  RichTextConfig (current isFocused <&> bool normal highlighted)

serversView
  :: forall t m. (MonadVtyApp t m, MonadNodeId m)
  => Dynamic t (Maybe GuildId)
  -> Dynamic t (Map.Map GuildId GuildState)
  -> VtyWidget t m (ReflexEvent t GuildId)
serversView selected guilds = do
  let d gs = fmap (fmap getFirst . mconcat)
  up <- keys [V.KUp, V.KChar 'k']
  down <- keys [V.KDown, V.KChar 'j']
  let selIndex = ffor2 selected guilds \sel g -> join (liftM2 Map.lookupIndex sel (pure g))
  e <- networkView $
    ffor3 selIndex selected (guildsList guilds) \ix sel g ->
      runLayout
        (constDyn Orientation_Row)
        (maybe 0 id ix)
        (leftmost
          [ (-1) <$ up
          , 1 <$ down
          ])
        g
  e' <- switchHold never (fmap (fmapMaybe id . updated) e)
  pure e'
  where
  guildsList guilds =
    guilds <&> \gs ->
      fmap (fmap getFirst . mconcat) $ forM (Map.toList gs) $ \(gId, guild) -> do
        stretch $ do
          f <- focus
          richText (highlight f) (pure (_guildName' guild))
          pure (bool (First Nothing) (First $ Just gId) <$> f)

channelsView
  :: forall t m. (MonadVtyApp t m, MonadNodeId m)
  => Dynamic t (Maybe ChannelId)
  -> Dynamic t (Maybe (Map.Map ChannelId ChannelState))
  -> VtyWidget t m (ReflexEvent t ChannelId)
channelsView selected channels = do
  up <- keys [V.KUp, V.KChar 'k']
  down <- keys [V.KDown, V.KChar 'j']
  let selIndex = ffor2 selected channels \sel ch -> join (liftM2 Map.lookupIndex sel ch)
  e <- networkView $
    ffor3 selIndex selected (channelsList channels) \ix sel ch ->
      runLayout
        (constDyn Orientation_Column)
        (maybe 0 id ix)
        (leftmost
          [ (-1) <$ up
          , 1 <$ down
          ])
        ch
  switchHold never $ fmap (fmapMaybe id) e
  where
  channelsList chans =
    chans <&> \chs ->
      case chs of
        Nothing -> pure never
        Just c -> fmap (updated . fmap getFirst . mconcat) $ forM (Map.toList c) \(cId, chan) -> do
          fixed 1 $ do
            inp <- input
            f <- focus
            richText (highlight f) (pure (_channelName' chan))
            pure (bool (First Nothing) (First $ Just cId) <$> f)

sendMessageWidget
  :: (Reflex t, MonadHold t m, MonadFix m, MonadNodeId m)
  => VtyWidget t m (ReflexEvent t Text)
sendMessageWidget = do
  send <- key V.KEnter
  textInp <- textInput (def { _textInputConfig_modify = const empty <$ send })
  let userInput = current (textInp & _textInput_value) `tag` send
  pure userInput

channelView
  :: (Reflex t, MonadHold t m, MonadFix m, MonadNodeId m)
  => Dynamic t (Maybe ChannelState)
  -> VtyWidget t m (ReflexEvent t Text)
channelView chanState = mdo
  (progressB, userSend) <- splitV
    (pure (subtract 1))
    (pure (False, True))
    (scrollableTextWindowed
      never
      (csToLines <$> chanState))
    sendMessageWidget
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

scrollableTextWindowed
  :: forall t m. (MonadHold t m, MonadFix m, Reflex t, MonadNodeId m)
  => ReflexEvent t Int
  -> Dynamic t [Text]
  -> VtyWidget t m (Behavior t (Int, Int, Int))
scrollableTextWindowed scrollBy contents = mdo
  f <- focus
  aw <- displayWidth
  ah <- displayHeight
  result <- pane
    (widgetRegion aw ah (snd result))
    f
    (scrollableText' scrollBy contents)
  pure (fst result)
  where
  widgetRegion aw ah widgetResult =
    DynRegion
      (constDyn 0)
      ((\x y -> max 0 (x - y)) <$> ah <*> widgetResult)
      aw
      (min <$> widgetResult <*> ah)

scrollableText'
  :: forall t m. (MonadHold t m, MonadFix m, Reflex t, MonadNodeId m)
  => ReflexEvent t Int
  -> Dynamic t [Text]
  -> VtyWidget t m (Behavior t (Int, Int, Int), Dynamic t Int)
scrollableText' scrollBy contents = do
  dw <- displayWidth
  dh <- displayHeight
  let imgs = wrap <$> dw <*> (T.intercalate "\n" <$> contents)
  kup <- key V.KUp
  kdown <- key V.KDown
  m <- mouseScroll
  let requestedScroll :: ReflexEvent t Int
      requestedScroll = leftmost
        [ 1 <$ kdown
        , (-1) <$ kup
        , ffor m $ \case
            ScrollDirection_Up -> (-1)
            ScrollDirection_Down -> 1
        , scrollBy
        ]
      updateLine maxN delta ix h = max 0 (min (maxN - h) (ix + delta))
  lineIndex :: Dynamic t Int <- foldDyn (\(h, (maxN, delta)) ix -> updateLine maxN delta ix h) 0 $
    attachPromptlyDyn dh $ attachPromptlyDyn (length <$> imgs) requestedScroll
  tellImages $ fmap ((:[]) . V.vertCat) $ current $ drop <$> lineIndex <*> imgs
  let result = (,,)
        <$> ((+) <$> current lineIndex <*> pure 1)
        <*> ((+) <$> current lineIndex <*> current dh)
        <*> (length <$> current imgs)
  pure (result, length <$> imgs)
  where
  wrap maxWidth =
    concatMap (fmap (V.string V.defAttr . T.unpack) . wrapWithOffset maxWidth 0) .
    T.split (=='\n')
