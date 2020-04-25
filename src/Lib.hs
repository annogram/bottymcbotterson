{-# Language OverloadedStrings #-}
module Lib
    ( botstart
    ) where
import Control.Monad        (when)
import System.Environment   (getEnv)
import Events               (eventPool)
import Botty.Event
import Data.Monoid
import Discord
import Discord.Types
import Control.Concurrent
import Control.Concurrent.STM
import qualified Discord.Requests as R
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import qualified Data.Map.Lazy as M

-- The program entry point, this function will grab the environment key and attach the event handler
botstart :: IO ()
botstart = do 
    putStrLn "Starting bot..."
    token <- T.pack <$> getEnv "DISCORD_CLIENT_SECRET"
    h <- atomically $ newTVar persistent
    userFacing <- runDiscord $ def 
                    { discordToken = token
                    , discordOnEvent = eventHandler h
                    , discordOnEnd = putStrLn "Bot terminating."
                    , discordOnStart = \handle -> putStrLn "...Bot started"
                    , discordForkThreadForEvents = True
                    }
    TIO.putStrLn userFacing
    where persistent :: M.Map Int String
          persistent = M.fromList []

-- The event handler will be passed to the discord client and execute the comands in the event module
eventHandler :: Persistent -> DiscordHandle -> Event -> IO ()
eventHandler p handle event = case event of 
    MessageCreate m -> botFilter m Nothing $ do
        case (getCommandStart m) `M.lookup` eventPool of
            Nothing     -> pure ()
            Just (f)    -> do
                let command  = messageText m
                _ <- logEvent handle m
                succ <- f command p
                case succ of
                    Just (text) -> do
                        seen handle m
                        result <- restCall handle $ R.CreateMessage (messageChannel m) $ text
                        shouldFollowUp result
                        pure ()
                    Nothing -> addReaction "thumbsdown" handle m
    _ -> pure ()
    where getCommandStart = head . T.words . T.toLower . messageText

shouldFollowUp :: Either RestCallErrorCode Message -> IO ()
shouldFollowUp res = case res of
    Left _ -> pure ()
    Right m -> pure ()

logEvent :: DiscordHandle -> Message -> IO ()
logEvent handle m = do
    let thisUser = userName . messageAuthor $ m
        command  = messageText m
    prefix <- case (R.GetGuild) <$> messageGuild m of
        Nothing       -> return $ "(private message) "
        Just (server) -> do
            Right (guild) <- restCall handle $ server
            return $ "(" <> (T.unpack . guildName) guild <> ") "
    putStrLn $ prefix
        <> "Event from user: " <> T.unpack thisUser
        <> " with command: " <> T.unpack command

seen:: DiscordHandle -> Message -> IO ()
seen = addReaction "ok_hand"

addReaction :: T.Text -> DiscordHandle -> Message -> IO ()
addReaction emoji handle m = do
                               _ <- restCall handle $ R.CreateReaction (messageChannel m, messageId m) emoji
                               pure ()

unSeen :: DiscordHandle -> Message -> IO ()
unSeen handle m = do
                   _ <- restCall handle $ R.DeleteAllReactions (messageChannel m, messageId m)
                   pure ()

-- The bot command query will filter all messages that do not meet the criteria for the bot to respond to
botCommandQuery :: Message -> Bool
botCommandQuery m = (notElem (messageText m) $ defaultCommand) && 
                        (not . userIsBot $ messageAuthor m)
    where defaultCommand = [ "/giphy"
                           , "/tenor"
                           , "/tts"
                           , "/me"
                           , "/tableflip"
                           , "/unflip"
                           , "/shrug"
                           , "/spoiler"
                           , "/nick" ]

-- Monadic mapping of the command condition to the when clause
botFilter ::Applicative f => Message -> Maybe Bool -> f () -> f ()
botFilter m (Just condition) f = when (botCommandQuery m && condition) $ f
botFilter m Nothing f = when (botCommandQuery m) $ f
