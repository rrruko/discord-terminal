{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module DiscordVty
  ( app
  , channelView
  , scrollableText'
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
import Reflex.Vty
import Reflex.Vty.Widget
import System.Environment

data Dir = Up | Down

type ReflexEvent = Reflex.Vty.Event
type DiscordEvent = Discord.Types.Event

data AppState = AppState
  { guilds :: Map.Map GuildId GuildState
  } deriving (Show)

data GuildState = GuildState
  { guildName :: Text
  , channels :: Map.Map ChannelId ChannelState
  } deriving (Show)

data ChannelState = ChannelState
  { channelName' :: Text
  , messages :: [AppMessage]
  } deriving (Show)

data AppMessage = AppMessage
  { author :: Text
  , contents :: Text
  , timestamp :: UTCTime
  } deriving (Show)

app :: IO ()
app =
  getArgs >>= \case
    (botToken : _) -> runClient (pack botToken)
    _ -> putStrLn "No token supplied."

runClient :: Text -> IO ()
runClient token = mainWidget widget
  where
  widget :: (MonadVtyApp t m, MonadNodeId m) => VtyWidget t m (ReflexEvent t ())
  widget = do
    inp <- input
    tog <- toggle True =<< tabNavigation
    let foc = fmap (\b -> if b then (True, False) else (False, True)) tog
    let guilds = Map.fromList $
          zip [0..] (["Server A", "Server B", "Server C"] :: [Text])
    mdo
      (_, currentGuildId) <- splitH
        (pure (subtract 12))
        foc
        (boxTitle (constant def) " Server view " (channelView chanState))
        (boxTitle (constant def) " Servers " (serversView guilds))
      let chanState = currentGuildId <&> (>>= (fakeServer Map.!?))
      pure ()
    pure $ fforMaybe inp $ \case
      V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
      _ -> Nothing
  fakeServer :: Map.Map ChannelId ChannelState
  fakeServer = Map.fromList
    [ (0, ChannelState "Channel 1" $
        [ AppMessage "buko" "hey" epochTime
        , AppMessage "buko" "wat up lol" epochTime
        , AppMessage "cardenas69" "can't talk right now... im draining" epochTime
        ])
    , (1, ChannelState "Channel 2" $
        [ AppMessage "eratosthenes" (T.concat $ replicate 100 "what's up gamers, ") epochTime
        ])
    ]

serversView
  :: forall t m. (MonadVtyApp t m, MonadNodeId m)
  => Map.Map GuildId Text
  -> VtyWidget t m (Dynamic t (Maybe GuildId))
serversView guildsMap = do
  inp <- input <&> ffilter (\x -> x == V.EvKey V.KEnter [])
  selectedGuildIx <- foldDyn (\_ acc -> mod (acc + 1) (length guildsMap)) 0 inp
  let selectedGuild = selectedGuildIx <&> (\ix -> elemAt' ix guildsMap)
  runLayout
    (constDyn Orientation_Column)
    0
    (1 <$ inp)
    serversList
  display (current $ fmap (fmap fst) selectedGuild)
  pure (fmap (fmap fst) selectedGuild)
  where
  normal = V.defAttr
  highlighted = V.withStyle V.defAttr V.standout
  highlight isFocused =
    RichTextConfig (current isFocused <&> bool normal highlighted)
  serversList = mdo
    forM (Map.toList guildsMap) \(id, name) -> do
      fixed 1 $ do
        f <- focus
        richText (highlight f) (pure name)
  elemAt' n m =
    let
      rest = snd (Map.splitAt n m)
    in
      if length rest > 0
        then Just (Map.elemAt 0 rest)
        else Nothing

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
      scrollableText'
        (1 <$ updated chanState)
        (csToLines <$> chanState))
    sendMessageWidget
  pure userSend
  where
    csToLines :: Maybe ChannelState -> [Text]
    csToLines (Just m) = fmap prettyMessage (messages m)
    csToLines Nothing = ["Failed to get channel state"]
    prettyMessage :: AppMessage -> Text
    prettyMessage msg =
      T.pack (show (timestamp msg)) <>
      "\n" <>
      author msg <>
      ": " <>
      contents msg <>
      "\n"

scrollableText'
  :: forall t m. (MonadHold t m, MonadFix m, Reflex t, MonadNodeId m)
  => ReflexEvent t Int -> Dynamic t [Text]
  -> VtyWidget t m (Behavior t (Int, Int, Int))
scrollableText' scrollBy contents = do
  f <- focus
  aw <- displayWidth
  ah <- displayHeight
  mdo
    result <- pane (widgetRegion aw ah (snd result)) f w
    pure (fst result)
  where
    w = do
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
    wrap maxWidth =
      concatMap (fmap (V.string V.defAttr . T.unpack) . wrapWithOffset maxWidth 0) .
      T.split (=='\n')
    widgetRegion aw ah widgetResult =
      DynRegion
        (constDyn 0)
        ((\x y -> max 0 (x - y)) <$> ah <*> widgetResult)
        aw
        (min <$> widgetResult <*> ah)

  {-
    DiscordEvents discordEvent discordStartEvent <- setupDiscord token
    inp <- input
    currentGuilds <- holdDyn Nothing
      (fmapMaybe (Just . getGuildsEvent) discordStartEvent)
    handle <- holdDyn Nothing
      (fmapMaybe (Just . getHandleEvent) discordStartEvent)
    let
      choice = fforMaybe inp $ \case
        V.EvKey (V.KLeft) [] -> Just Up
        V.EvKey (V.KRight) [] -> Just Down
        _ -> Nothing
    focusedGuild <-
      foldDyn
        (\(guilds, dir) (guildIx, guild) -> do
          let ix = case dir of
                Up -> mod (guildIx + 1) (length guilds)
                Down -> mod (guildIx - 1) (length guilds)
          (ix, guilds !! ix))
        (0, Nothing)
        (attachPromptlyDyn currentGuilds choice)
    let
      handleWithGuild = fmapMaybe (\(x, y) -> (\z -> (z,y)) <$> x) $
        attachPromptlyDyn handle (fmapMaybe id (updated focusedGuild))
    statuses <- performEventAsync (uncurry getGuildChannels <$> handleWithGuild)
    display =<< hold [] statuses
    pure $ fforMaybe inp $ \case
      V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
      _ -> Nothing
  -}

keyPressed
  :: (Reflex t)
  => V.Key -> ReflexEvent t VtyEvent -> ReflexEvent t ()
keyPressed k ev = fforMaybe ev $ \case
  V.EvKey key _ | key == k -> Just ()
  _ -> Nothing

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
    Right guilds -> callback (DiscordGuildsEvent guilds)

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
  | DiscordGuildsEvent [PartialGuild]

getHandleEvent :: DiscordStartEvent -> Maybe DiscordHandle
getHandleEvent = \case
  DiscordHandleEvent h -> Just h
  _ -> Nothing

getGuildsEvent :: DiscordStartEvent -> Maybe [PartialGuild]
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
