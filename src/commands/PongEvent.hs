{-# Language OverloadedStrings #-}
module PongEvent
    ( pongResp
    , pongCommand
    ) where
import Discord
import Discord.Types
import Data.Text
import qualified Discord.Requests as R

-- Exposed comamnd key
pongCommand :: Text
pongCommand = "/ping"

-- Functionality to reply to ping
pongResp :: DiscordHandle -> Event -> IO ()
pongResp handle (MessageCreate m) = do 
    _ <- restCall handle $  R.CreateMessage (messageChannel m) $ "Pong!"
    pure ()