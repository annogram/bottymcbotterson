{-# Language OverloadedStrings, ViewPatterns #-}
module Lib
    ( botstart
    ) where
import Control.Monad         (when)
import System.Environment    (getEnv)
import Botty.Commands.Events (eventPool, followUpPool, vote, unvote)
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
import qualified Data.ByteString as BS

-- | The program entry point, this function will grab the environment key and attach the event handler
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
                    , discordOnLog = print
                    }
    TIO.putStrLn userFacing
    botstart
    where persistent = M.empty :: M.Map Int BS.ByteString

getCommandStart = head . T.words . T.toLower . messageText

-- | The event handler will be passed to the discord client and execute the comands in the event module
eventHandler :: Persistent -> DiscordHandle -> Event -> IO ()
eventHandler p handle (MessageCreate m) = botFilter m Nothing $ do
    case (getCommandStart m) `M.lookup` eventPool of
        Nothing  -> pure ()
        Just (f) -> do
            seen handle m
            let command  = messageText m
            logEvent handle m
            succ <- f command p
            case succ of
                Just (text) -> do
                    result <- restCall handle $ R.CreateMessage (messageChannel m) $ text
                    shouldFollowUp handle result (getCommandStart m) p
                    pure ()
                Nothing -> do
                    removeReaction "ok_hand" handle m
                    addReaction "thumbsdown" handle m
eventHandler p handle (MessageReactionAdd ri) = do
    Right (callingUser) <- restCall handle $ R.GetUser (reactionUserId ri)
    when (not . userIsBot $ callingUser) $ do
        Right (m) <- restCall handle $ R.GetChannelMessage (reactionChannelId ri, reactionMessageId ri)
        when (userIsBot (messageAuthor m)) $ do
            _ <- vote handle ri p
            pure ()
eventHandler p handle (MessageReactionRemove ri) = do
    Right (callingUser) <- restCall handle $ R.GetUser (reactionUserId ri)
    when (not . userIsBot $ callingUser) $ do
        Right (m) <- restCall handle $ R.GetChannelMessage (reactionChannelId ri, reactionMessageId ri)
        when (userIsBot (messageAuthor m)) $ do
            _ <- unvote handle ri p
            pure ()
eventHandler p handle (_) = pure ()

-- | Handle followups for commands that need it
shouldFollowUp :: DiscordHandle -> Either RestCallErrorCode Message -> T.Text -> Persistent -> IO ()
shouldFollowUp h (Left _) cmd p = pure ()
shouldFollowUp h (Right m) cmd p = case cmd `M.lookup` followUpPool of
    Nothing -> pure ()
    Just (f) -> do
        succ <- f h m cmd p
        pure ()

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

removeReaction :: T.Text -> DiscordHandle -> Message -> IO ()
removeReaction emoji handle m = do
    _ <- restCall handle $ R.DeleteOwnReaction (messageChannel m, messageId m) emoji
    return ()

seen:: DiscordHandle -> Message -> IO ()
seen = addReaction "ok_hand"

addReaction :: T.Text -> DiscordHandle -> Message -> IO ()
addReaction emoji handle m = do
                               _ <- restCall handle $ R.CreateReaction (messageChannel m, messageId m) emoji
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

-- | Write the current state of persistent storage to disk
writePersistence :: Persistent -> IO ()
writePersistence = undefined