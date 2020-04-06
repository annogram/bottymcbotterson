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
            token <- T.pack <$> (getEnv $ "DISCORD_CLIENT_SECRET")
            userFacing <- runDiscord $ def 
                            { discordToken = token
                            , discordOnEvent = eventHandler
                            , discordOnStart = handleStart }
            TIO.putStrLn userFacing

eventHandler :: DiscordHandle -> Event -> IO ()
eventHandler handle event = case event of 
    _ -> pure ()

handleStart :: DiscordHandle -> IO ()
handleStart handle = putStrLn "Bot started"