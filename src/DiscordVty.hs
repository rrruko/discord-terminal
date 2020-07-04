{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}

module DiscordVty
  ( app
  , channelView
  , scrollableTextWindowed
  ) where

import Control.Concurrent (forkIO, threadDelay)
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

data ChannelState = ChannelState
  { channelName' :: Text
  , messages :: Lazy [AppMessage]
  } deriving (Show)

data AppMessage = AppMessage
  { author :: Text
  , contents :: Text
  , timestamp :: UTCTime
  } deriving (Show)

data GuildState = GuildState
  { guildName' :: Text
  , channels :: Map.Map ChannelId ChannelState
  }

data AppState = AppState
  { guildsMap :: Map.Map GuildId GuildState
  }

data Lazy a
  = NotLoaded
  | Loaded a
  deriving (Eq, Ord, Show)

app :: IO ()
app =
  getArgs >>= \case
    (botToken : _) -> runClient (pack botToken)
    _ -> putStrLn "No token supplied."

runClient :: Text -> IO ()
runClient token = mainWidget do
  DiscordEvents discordEvent discordStartEvent <- setupDiscord token
  guilds <- holdDyn (AppState mempty)
    (fmapMaybe getGuildsEvent discordStartEvent)
  handle <- holdDyn Nothing
    (fmapMaybe (Just . getHandleEvent) discordStartEvent)
  tog <- toggle True =<< tabNavigation
  let foc = fmap (bool (False, True) (True, False)) tog
  currChanId <- serverWidget foc guilds sendUserMessage
  reqChannelMessages <- debounce 0.5 currChanId
    <&> fmapMaybe (\(a,b,c) -> guard (not c) *> Just (a,b))
  let handleAvailable = fmapMaybe id (updated handle)
  newMessages <- switchDyn <$> networkHold
    (pure never)
    ((\h -> performEventAsync (fmap (uncurry (requestChannelMessages h)) reqChannelMessages))
      <$> handleAvailable)
  inp <- input
  pure $ fforMaybe inp $ \case
    V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
    _ -> Nothing
  where
  sendUserMessage :: MonadIO m => Text -> m ()
  sendUserMessage text = pure ()

requestChannelMessages
  :: MonadIO m
  => DiscordHandle
  -> GuildId
  -> ChannelId
  -> ([Message] -> IO ())
  -> m ()
requestChannelMessages handle gId cId callback = liftIO $
  restCall handle (R.GetChannelMessages cId (10, R.LatestMessages)) >>= \case
    Left errCode -> pure ()
    Right msgs -> callback msgs

handleDiscordEvent
  :: MonadIO m
  => DiscordHandle -> DiscordEvent -> (() -> IO ()) -> m ()
handleDiscordEvent handle ev callback = do
  pure ()

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
  { event :: ReflexEvent t ()
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

getGuildChannels
  :: MonadIO m
  => DiscordHandle -> PartialGuild -> ([Channel] -> IO ()) -> m ()
getGuildChannels handle guild callback = liftIO $ do
  let command = R.GetGuildChannels (partialGuildId guild)
  restCall handle command >>= \case
    Left errCode -> print errCode
    Right channels -> callback channels

channelMessages
  :: MonadIO m
  => DiscordHandle -> Channel -> (Maybe [Text] -> IO ()) -> m ()
channelMessages handle channel callback = liftIO $ do
  let chanId = channelId channel
  let command = R.GetChannelMessages chanId (5, R.LatestMessages)
  restCall handle command >>= \case
    Left errCode -> callback Nothing
    Right messages -> do
      callback (Just (fmap messageText messages))

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

serverWidget
  :: (MonadVtyApp t m, MonadNodeId m)
  => Dynamic t (Bool, Bool)
  -> Dynamic t AppState
  -> (Text -> ReaderT (VtyWidgetCtx t) (Performable m) ())
  -> VtyWidget t m (ReflexEvent t (GuildId, ChannelId, Bool))
serverWidget foc guilds sendUserMessage = mdo
  inp <- key V.KEsc
  tog <- toggle False inp
  (newGuildId, (userSend, newChanId)) <-
    splitV (pure (const 1)) (tog <&> bool (True, False) (False, True))
      (serversView (fmap guildsMap guilds))
      (splitH
          (pure (subtract 12))
          foc
          (boxTitle (constant def) " Channel view "
            (channelView chanState))
          (boxTitle (constant def) " Channels "
            (channelsView (fmap (fmap channels) currentGuild))))

  let initGuildId = guilds <&> (First . fmap fst . elemAt' 0 . guildsMap)
  updatedGuildId <- fmap First <$> foldDyn (\x acc -> Just x) Nothing newGuildId
  let currentGuildId = getFirst <$> mconcat
        [ updatedGuildId, initGuildId ]
  let initChanId = currentGuild <&> (First . fmap fst . (>>= elemAt' 0 . channels))
  updatedChanId <- fmap First <$> foldDyn (\x acc -> Just x) Nothing newChanId
  let currentChanId = getFirst <$> mconcat
        [ updatedChanId, initChanId ]
  let currentGuild = (\g gId -> gId >>= \i -> (guildsMap g) Map.!? i) <$> guilds <*> currentGuildId

  let chanState = (\s gId cId -> getChannelState s gId cId)
        <$> (fmap guildsMap guilds)
        <*> currentGuildId
        <*> currentChanId
  performEvent_ (fmap sendUserMessage userSend)
  let isLoaded = fmap
        (\case { Just (ChannelState _ NotLoaded) -> False; _ -> True })
        chanState
  let updatedGuildChanId = (,,) <$> currentGuildId <*> currentChanId <*> isLoaded
  pure (fforMaybe (updated updatedGuildChanId) (\(a,b,c) -> (,,) <$> a <*> b <*> Just c))

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
  channels currGuild Map.!? c

normal = V.defAttr
highlighted = V.withStyle V.defAttr V.standout
highlight isFocused =
  RichTextConfig (current isFocused <&> bool normal highlighted)

serversView
  :: forall t m. (MonadVtyApp t m, MonadNodeId m)
  => Dynamic t (Map.Map GuildId GuildState)
  -> VtyWidget t m (ReflexEvent t GuildId)
serversView guilds = do
  e <- networkView $ guilds <&> \gs -> do
    inp <- key V.KEnter
    runLayout
      (constDyn Orientation_Row)
      0
      (1 <$ inp)
      $ fmap (fmap getFirst . mconcat) $ forM (Map.toList gs) $ \(gId, guild) -> do
        stretch $ do
          f <- focus
          richText (highlight f) (pure (guildName' guild))
          pure (bool (First Nothing) (First $ Just gId) <$> f)
  e' <- switchHold never (fmap (fmapMaybe id . updated) e)
  pure e'

channelsView
  :: forall t m. (MonadVtyApp t m, MonadNodeId m)
  => Dynamic t (Maybe (Map.Map ChannelId ChannelState))
  -> VtyWidget t m (ReflexEvent t ChannelId)
channelsView channels = do
  inp <- input
  e <- networkView $
    runLayout
      (constDyn Orientation_Column)
      0
      (1 <$ inp)
      <$> (channelsList channels)
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
            richText (highlight f) (pure (channelName' chan))
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
    (do
      display ((("    " <>) . show) <$> progressB)
      scrollableTextWindowed
        (1 <$ updated chanState)
        (csToLines <$> chanState))
    sendMessageWidget
  pure userSend
  where
    csToLines :: Maybe ChannelState -> [Text]
    csToLines (Just m) =
      case messages m of
        Loaded m' -> fmap prettyMessage m'
        NotLoaded -> ["Not loaded"]
    csToLines Nothing = ["Failed to get channel state"]
    prettyMessage :: AppMessage -> Text
    prettyMessage msg =
      T.pack (show (timestamp msg)) <>
      "\n" <>
      author msg <>
      ": " <>
      contents msg <>
      "\n"

scrollableTextWindowed
  :: forall t m. (MonadHold t m, MonadFix m, Reflex t, MonadNodeId m)
  => ReflexEvent t Int -> Dynamic t [Text]
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
  => ReflexEvent t Int -> Dynamic t [Text]
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
