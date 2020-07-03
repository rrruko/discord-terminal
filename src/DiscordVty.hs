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
runClient token = mainWidget do
  tog <- toggle True =<< tabNavigation
  let foc = fmap (bool (False, True) (True, False)) tog
  let guilds = Map.fromList $
        zip [0..] ["Server A", "Server B", "Server C"]
  serverWidget foc (pure guilds) (pure fakeServer) sendUserMessage
  inp <- input
  pure $ fforMaybe inp $ \case
    V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
    _ -> Nothing
  where
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
  sendUserMessage :: MonadIO m => Text -> m ()
  sendUserMessage text = pure ()

serverWidget
  :: (MonadVtyApp t m, MonadNodeId m)
  => Dynamic t (Bool, Bool)
  -> Dynamic t (Map.Map GuildId Text)
  -> Dynamic t (Map.Map ChannelId ChannelState)
  -> (Text -> ReaderT (VtyWidgetCtx t) (Performable m) ())
  -> VtyWidget t m ()
serverWidget foc guilds server sendUserMessage = mdo
  (userSend, currentGuildId) <- splitH
    (pure (subtract 12))
    foc
    (boxTitle (constant def) " Server view " (channelView chanState))
    (boxTitle (constant def) " Servers " (serversView guilds))
  let chanState = (\s ident -> ident >>= (s Map.!?)) <$> server <*> currentGuildId
  performEvent_ (fmap sendUserMessage userSend)

serversView
  :: forall t m. (MonadVtyApp t m, MonadNodeId m)
  => Dynamic t (Map.Map GuildId Text)
  -> VtyWidget t m (Dynamic t (Maybe GuildId))
serversView guildsMap = do
  inp <- input <&> ffilter (== V.EvKey V.KEnter [])
  selectedGuildIx <-
    foldDyn (\l acc -> mod (acc + 1) l) 0 (current (fmap length guildsMap) <@ inp)
  let selectedGuild = elemAt' <$> selectedGuildIx <*> guildsMap
  networkView $
    runLayout
      (constDyn Orientation_Column)
      0
      (1 <$ inp)
      <$> serversList
  display (current $ fmap (fmap fst) selectedGuild)
  pure (fmap (fmap fst) selectedGuild)
  where
  normal = V.defAttr
  highlighted = V.withStyle V.defAttr V.standout
  highlight isFocused =
    RichTextConfig (current isFocused <&> bool normal highlighted)
  serversList :: Dynamic t (Layout t m [()])
  serversList =
    guildsMap <&> \gm -> forM (Map.toList gm) \(id, name) -> do
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
      scrollableTextWindowed
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
