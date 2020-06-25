{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (when, void)
import Control.Monad.IO.Class
import qualified Data.Set as Set
import Data.Text (Text, intercalate, isPrefixOf, pack, toLower)
import qualified Data.Text.IO as TIO
import Discord
import qualified Discord.Requests as R
import Discord.Types
import qualified Graphics.Vty as V
import Reflex
import Reflex.Vty
import System.Environment

data Dir = Up | Down

main :: IO ()
main =
  getArgs >>= \case
    (botToken : _) -> runClient (pack botToken)
    _ -> putStrLn "No token supplied."

runClient :: Text -> IO ()
runClient token =
  mainWidget $ do
    (discordEvent, triggerDiscordEvent) <- newTriggerEvent
    (discordStartEvent, onStartEvent) <- newTriggerEvent
    start <- getPostBuild
    performEventAsync
      (setupDiscord token triggerDiscordEvent onStartEvent <$ start)

    inp <- input
    performEventAsync (fmap (uncurry handleDiscordEvent) discordEvent)
    ev <- performEventAsync (fmap handleOnStartEvent discordStartEvent)
    currentGuilds <- holdDyn Nothing (fmap Set.fromList <$> ev)
    display =<< hold Nothing ev
    let
      choice = fforMaybe inp $ \case
        V.EvKey (V.KLeft) [] -> Just Up
        V.EvKey (V.KRight) [] -> Just Down
    focusedGuild <-
      foldDyn
        (\(guilds, dir) currentGuild -> do
          case currentGuild of
            Nothing -> fmap (Set.elemAt 0) guilds
            Just curr -> do
              gs <- guilds
              pure $
                case dir of
                  Up -> Set.elemAt 0 (snd (Set.split curr gs))
                  Down -> Set.elemAt 0 (fst (Set.split curr gs)))
        Nothing
        (attachPromptlyDyn currentGuilds choice)
    pure $ fforMaybe inp $ \case
      V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
      _ -> Nothing

handleDiscordEvent :: MonadIO m => DiscordHandle -> Discord.Types.Event -> (() -> IO ()) -> m ()
handleDiscordEvent handle ev callback = do
  pure ()

handleOnStartEvent :: MonadIO m => DiscordHandle -> (Maybe [Text] -> IO ()) -> m ()
handleOnStartEvent handle callback = liftIO $ do
  restCall handle R.GetCurrentUserGuilds >>= \case
    Left errCode -> callback Nothing
    Right guilds -> callback (Just (fmap partialGuildName guilds))

type TriggerDiscordEvent = (DiscordHandle, Discord.Types.Event) -> IO ()
type OnStartEvent = DiscordHandle -> IO ()

setupDiscord :: MonadIO m => Text -> TriggerDiscordEvent -> OnStartEvent -> (Text -> IO ()) -> m ()
setupDiscord botToken triggerDiscordEvent onStartEvent callback = liftIO $ void $ forkIO $ do
  err <- runDiscord $ def
           { discordToken = botToken
           , discordOnEvent = curry triggerDiscordEvent
           , discordOnStart = onStartEvent
           }
  callback err

showGuildStatus :: DiscordHandle -> PartialGuild -> IO ()
showGuildStatus handle guild = do
  restCall handle (R.GetGuildChannels (partialGuildId guild)) >>= \case
    Left errCode -> print errCode
    Right channels -> do
      traverse (showChannelStatus handle) channels
      pure ()
  TIO.putStrLn (partialGuildName guild)

showChannelStatus :: DiscordHandle -> Channel -> IO ()
showChannelStatus handle channel = do
  TIO.putStrLn ("  " <> channelName' channel)
  restCall handle (R.GetChannelMessages (channelId channel) (5, R.LatestMessages)) >>= \case
    Left errCode -> print errCode
    Right messages -> do
      traverse (TIO.putStrLn . ("    " <>) . messageText) messages
      pure ()

channelName' :: Channel -> Text
channelName' c = case c of
  ChannelText {} -> channelName c
  ChannelVoice {} -> channelName c
  ChannelDirectMessage {} -> intercalate ", " (fmap userName $ channelRecipients c)
  ChannelGroupDM {} -> intercalate ", " (fmap userName $ channelRecipients c)
  ChannelGuildCategory {} -> "<Category>"
