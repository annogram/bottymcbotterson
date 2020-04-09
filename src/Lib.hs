{-# Language OverloadedStrings #-}
module Lib
    ( botstart
    ) where
import Control.Monad        (when)
import System.Environment   (getEnv)
import Events               (eventPool)
import Control.Concurrent   (threadDelay)
import Data.Monoid
import Discord
import Discord.Types
import qualified Discord.Requests as R
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import qualified Data.Map.Lazy as M

-- The program entry point, this function will grab the environment key and attach the event handler
botstart :: IO ()
botstart = do 
            putStrLn "Starting bot..."
            token <- T.pack <$> getEnv "DISCORD_CLIENT_SECRET"
            userFacing <- runDiscord $ def 
                            { discordToken = token
                            , discordOnEvent = eventHandler
                            , discordOnEnd = putStrLn "Bot terminating."
                            , discordOnStart = \handle -> putStrLn "...Bot started"
                            , discordForkThreadForEvents = True }
            TIO.putStrLn userFacing

-- The event handler will be passed to the discord client and execute the comands in the event module
eventHandler :: DiscordHandle -> Event -> IO ()
eventHandler handle event = case event of 
    MessageCreate m -> botFilter m Nothing $ do
        case (getCommandStart m) `M.lookup` eventPool of
            Nothing     -> pure ()
            Just (f)    -> do
                            let thisUser = userName . messageAuthor $ m
                            let command  = messageText m
                            putStrLn $ "Event from user: " <> T.unpack thisUser
                                <> " with command: " <> T.unpack command
                            _ <- restCall handle $ R.CreateMessage (messageChannel m) 
                                $ "> Responding to <@" <> (T.pack . show . userId $ (messageAuthor m)) <> ">"
                            seen handle m
                            succ <- f handle event
                            if succ 
                                then pure ()
                                else addReaction "thumbsdown" handle m
    _ -> pure ()
    where getCommandStart = head . T.words . T.toLower . messageText

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
