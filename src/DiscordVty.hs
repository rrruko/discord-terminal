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
import Data.Function (on)
import Data.List (elemIndex, find, sortOn, partition)
import qualified Data.List.NonEmpty as NE
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
import Discord.Types hiding (Event)
import qualified Discord.Types
import qualified Graphics.Vty as V
import Reflex
import Reflex.Pure
import Reflex.Network
import Reflex.Vty
import Reflex.Vty.Widget
import System.Environment

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
  let handle = fmapMaybe getHandleEvent discordStartEvent
  networkHold
    (text "Acquiring handle...")
    (fmap (runAppWithHandle guilds discordEvent) handle)
  inp <- input
  pure $ fforMaybe inp $ \case
    V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
    _ -> Nothing

runAppWithHandle
  :: (MonadVtyApp t m, MonadNodeId m)
  => Dynamic t AppState
  -> Event t NewMessage
  -> DiscordHandle
  -> VtyWidget t m ()
runAppWithHandle guilds discordEvent handle = mdo
  currChanId <- serverWidget handle updatedAppState sendUserMessage
  updatedAppState <- updateAppState handle currChanId guilds discordEvent
  pure ()

sendUserMessage :: MonadIO m => (DiscordHandle, Text, ChannelId) -> m (Maybe RestCallErrorCode)
sendUserMessage (handle, text, cId) = liftIO $
  restCall handle (R.CreateMessage cId text) >>= \case
    Left errCode -> pure (Just errCode)
    Right _ -> pure Nothing

updateAppState
  :: (MonadVtyApp t m)
  => DiscordHandle
  -> Event t (GuildId, ChannelId, Bool)
  -> Dynamic t AppState
  -> Event t NewMessage
  -> VtyWidget t m (Dynamic t AppState)
updateAppState handle currChanId guilds newMsg = do
  reqChannelMessages <- debounce 0.5 currChanId
    <&> fmapMaybe (\(a,b,c) -> guard (not c) *> Just (a,b))
  let newCreatedMessages = getNewMessageContext (current guilds) newMsg
  newMessages <-
    (performEventAsync
      (fmap (uncurry (requestChannelMessages handle)) reqChannelMessages))
  let appStateUpdates = iterateEvent $ fmap (fmap updateMessages) $
        (mergeList [newMessages, newCreatedMessages])
  updatedAppStateDyn <- foldDyn (.) id appStateUpdates
  pure (updatedAppStateDyn <*> guilds)

getNewMessageContext
  :: (Reflex t)
  => Behavior t AppState
  -> Event t NewMessage
  -> Event t (GuildId, ChannelId, [AppMessage])
getNewMessageContext guilds newMsg = do
  let e = attach guilds newMsg
  fmapMaybe id $ ffor e (\(gs, m) -> do
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
      [ (partialGuildId g, GuildState (partialGuildName g) (channelMap Map.! partialGuildId g))
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

serverWidget
  :: (MonadVtyApp t m, MonadNodeId m)
  => DiscordHandle
  -> Dynamic t AppState
  -> ((DiscordHandle, Text, ChannelId) -> ReaderT (VtyWidgetCtx t) (Performable m) (Maybe RestCallErrorCode))
  -> VtyWidget t m (Event t (GuildId, ChannelId, Bool))
serverWidget handle guilds sendUserMessage = mdo
  inp <- key V.KEsc
  tog <- toggle False inp
  nav <- tabNavigation
  navTog <- toggle True nav
  let foc = fmap (bool (False, True) (True, False)) navTog
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

  (currentGuildId, currentChanId) <-
    accumulateGuildChannel
      currentGuild
      newGuildId
      newChanId
      guilds
  let currentGuild = (\g gId -> gId >>= \i -> (_guildsMap g) Map.!? i) <$> guilds <*> currentGuildId
  let chanState = (\s gId cId -> getChannelState s gId cId)
        <$> (fmap _guildsMap guilds)
        <*> currentGuildId
        <*> currentChanId

  let sendEv = attach (pure handle) (attach (current currentChanId) userSend)
  performEvent (sendUserMessage
    <$> (sendEv & fmapMaybe (\(a,(b,c)) -> (,,) <$> pure a <*> pure c <*> b)))

  let isLoaded = fmap
        (\case { Just (ChannelState _ _ _ _ _ NotLoaded) -> False; _ -> True })
        chanState
  let updatedGuildChanId = (,,) <$> currentGuildId <*> currentChanId <*> isLoaded
  pure (fforMaybe (updated updatedGuildChanId) (\(a,b,c) -> (,,) <$> a <*> b <*> Just c))

accumulateGuildChannel
  :: (Reflex t, MonadHold t m, MonadFix m)
  => Dynamic t (Maybe GuildState)
  -> Event t GuildId
  -> Event t ChannelId
  -> Dynamic t AppState
  -> m (Dynamic t (Maybe GuildId), Dynamic t (Maybe ChannelId))
accumulateGuildChannel currentGuild newGuildId newChanId guilds = do
  let initGuildId = guilds <&> (fmap fst . elemAt' 0 . _guildsMap)
  updatedGuildId <- foldDyn (\x acc -> Just x) Nothing newGuildId
  let currentGuildId = getFirst <$> mconcat
        (fmap (fmap First) [ updatedGuildId, initGuildId ])
  initChanId <- holdUniqDyn (currentGuild <&> (fmap fst . (>>= elemAt' 0 . DiscordVty._channels)))
  selectedChannels <-
    foldDyn
      (uncurry Map.insert)
      mempty
      (attachPromptlyDyn currentGuildId newChanId)
  let savedChanId = ffor2 selectedChannels currentGuildId (Map.!?)
  updatedChanId <- foldDyn (\x acc -> Just x) Nothing newChanId
  let currentChanId = ffor3 updatedChanId savedChanId initChanId \uC sC iC ->
        getFirst (foldMap First [sC, iC, uC])
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
  -> VtyWidget t m (Event t GuildId)
serversView selected guilds = join $ fmap (switchHold never) $ networkView $
  ffor2 selected guilds \sel gs ->
    case (sel, gs) of
      (Just s, g) ->
        optionList s g Orientation_Row const \gId guild ->
          stretch do
            f <- focus
            richText (highlight f) (pure (_guildName' guild))
            pure (bool (First Nothing) (First $ Just gId) <$> f)
      _ -> pure never

channelsView
  :: forall t m. (MonadVtyApp t m, MonadNodeId m)
  => Dynamic t (Maybe ChannelId)
  -> Dynamic t (Maybe (Map.Map ChannelId ChannelState))
  -> VtyWidget t m (Event t ChannelId)
channelsView selected channels = join $ fmap (switchHold never) $ networkView $
  ffor2 selected channels \sel ch ->
    case (sel, ch) of
      (Just s, Just c) ->
        optionList s c Orientation_Column (\k v -> sortKey' (Map.elems c) v) \cId channel ->
          fixed 1 do
            f <- focus
            richText (highlight f) (pure (_channelName' channel))
            pure (bool (First Nothing) (First $ Just cId) <$> f)
      _ -> pure never

sortKey' :: [ChannelState] -> ChannelState -> (Maybe Integer, Maybe Integer)
sortKey' chans this =
  let
    parentKey = _channelParentId' this
    parentChannel = find ((==parentKey) . Just . _channelId') chans
    parentPosition = _channelPosition' =<< parentChannel
    textChannelPosition
      | _channelIsCategory this = Nothing
      | otherwise = _channelPosition' this
  in
    (getFirst (foldMap First [parentPosition, _channelPosition' this])
    , textChannelPosition)

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
  fmapMaybe id <$> runLayout
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

sendMessageWidget
  :: (Reflex t, MonadHold t m, MonadFix m, MonadNodeId m)
  => VtyWidget t m (Event t Text)
sendMessageWidget = do
  send <- key V.KEnter
  textInp <- textInput' (def { _textInputConfig_modify = const empty <$ send })
  let userInput = current (textInp & _textInput_value) `tag` send
  pure userInput

channelView
  :: (Reflex t, MonadHold t m, MonadFix m, MonadNodeId m)
  => Dynamic t (Maybe ChannelState)
  -> VtyWidget t m (Event t Text)
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
  => Event t Int
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
  => Event t Int
  -> Dynamic t [Text]
  -> VtyWidget t m (Behavior t (Int, Int, Int), Dynamic t Int)
scrollableText' scrollBy contents = do
  dw <- displayWidth
  dh <- displayHeight
  let imgs = wrap <$> dw <*> (T.intercalate "\n" <$> contents)
  kup <- key V.KUp
  kdown <- key V.KDown
  m <- mouseScroll
  let requestedScroll :: Event t Int
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

-- This is just the reflex-vty textInput widget with tab inputs filtered out.
-- I'm using tab for navigation and I don't want to worry about keeping my
-- input widget unfocused when the user is trying to tab out.
textInput'
  :: (Reflex t, MonadHold t m, MonadFix m)
  => TextInputConfig t
  -> VtyWidget t m (TextInput t)
textInput' cfg = do
  i <- input <&> ffilter (\ev -> ev /= V.EvKey (V.KChar '\t') [])
  f <- focus
  dh <- displayHeight
  dw <- displayWidth
  rec v <- foldDyn ($) (_textInputConfig_initialValue cfg) $ mergeWith (.)
        [ uncurry (updateTextZipper (_textInputConfig_tabWidth cfg)) <$> attach (current dh) i
        , _textInputConfig_modify cfg
        , let displayInfo = (,) <$> current rows <*> scrollTop
          in ffor (attach displayInfo click) $ \((dl, st), MouseDown _ (mx, my) _) ->
            goToDisplayLinePosition mx (st + my) dl
        ]
      click <- mouseDown V.BLeft
      let cursorAttrs = ffor f $ \x -> if x then cursorAttributes else V.defAttr
      let rows = (\w s c -> displayLines w V.defAttr c s)
            <$> dw
            <*> (mapZipper <$> _textInputConfig_display cfg <*> v)
            <*> cursorAttrs
          img = images . _displayLines_spans <$> rows
      y <- holdUniqDyn $ _displayLines_cursorY <$> rows
      let newScrollTop :: Int -> (Int, Int) -> Int
          newScrollTop st (h, cursorY)
            | cursorY < st = cursorY
            | cursorY >= st + h = cursorY - h + 1
            | otherwise = st
      let hy = attachWith newScrollTop scrollTop $ updated $ zipDyn dh y
      scrollTop <- hold 0 hy
      tellImages $ (\imgs st -> (:[]) . V.vertCat $ drop st imgs) <$> current img <*> scrollTop
  return $ TextInput
    { _textInput_value = value <$> v
    , _textInput_lines = length . _displayLines_spans <$> rows
    }
