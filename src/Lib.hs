{-# Language OverloadedStrings #-}
module Lib
    ( botstart
    ) where
import Control.Monad (when)
import System.Environment (getEnv)
import Discord
import Discord.Types
import qualified Discord.Requests as R
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

botstart :: IO ()
botstart = do 
            putStrLn "Starting bot..."
            token <- T.pack <$> getEnv "DISCORD_CLIENT_SECRET"
            userFacing <- runDiscord $ def 
                            { discordToken = token
                            , discordOnEvent = eventHandler
                            , discordOnStart = \handle -> putStrLn "...Bot started" }
            TIO.putStrLn userFacing

eventHandler :: DiscordHandle -> Event -> IO ()
eventHandler handle event = case event of 
    MessageCreate m -> botFilter m Nothing $ do
        _ <- restCall handle $ R.CreateReaction (messageChannel m, messageId m) "eyes"
        pure ()
    _ -> pure ()

botFilter ::Applicative f => Message -> Maybe Bool -> f () -> f ()
botFilter m (Just condition) f = when ((userIsBot $ messageAuthor m ) && condition) $ f
botFilter m Nothing f = when ((userIsBot $ messageAuthor m )) $ f
