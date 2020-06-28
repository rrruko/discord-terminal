{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Lens.Operators
import Control.Monad (when, void)
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.Bitraversable
import qualified Data.Set as Set
import Data.Text (Text, intercalate, isPrefixOf, unlines, pack, toLower)
import qualified Data.Text.IO as TIO
import Data.Text.Zipper
import Discord
import qualified Discord.Requests as R
import Discord.Types
import qualified Graphics.Vty as V
import Reflex
import Reflex.Vty
import Reflex.Vty.Widget
import System.Environment

data Dir = Up | Down

type ReflexEvent = Reflex.Vty.Event
type DiscordEvent = Discord.Types.Event

main :: IO ()
main =
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
    splitH
      (pure (subtract 12))
      foc
      (boxTitle (constant def) " Server view " channelView)
      (boxTitle (constant def) " Servers " blank)
    pure $ fforMaybe inp $ \case
      V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
      _ -> Nothing

sendMessageWidget :: (MonadVtyApp t m, MonadNodeId m) => VtyWidget t m (Dynamic t [Text])
sendMessageWidget = do
  send <- key V.KEnter
  textInp <- textInput (def { _textInputConfig_modify = const empty <$ send })
  let userInput = current (textInp & _textInput_value) `tag` send
  history <- foldDyn (:) ["some stuff"] userInput
  pure history

channelView :: (MonadVtyApp t m, MonadNodeId m) => VtyWidget t m ()
channelView = mdo
  (_, history) <- splitV
    (pure (subtract 1))
    (pure (False, True))
    (scrollableText
      (1 <$ updated history)
      (current $ Data.Text.unlines . reverse <$> history))
    sendMessageWidget
  pure ()


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

channelsView
  :: (MonadVtyApp t m, MonadNodeId m)
  => Dynamic t [(PartialGuild, [Channel])]
  -> VtyWidget t m (ReflexEvent t ())
channelsView allChannels = do
  inp <- keyPressed V.KEnter <$> input
  pure never

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
