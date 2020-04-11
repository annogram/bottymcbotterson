{-# Language OverloadedStrings #-}
module PongEvent
    ( pongResp
    , pongCommand
    , pongDesc
    ) where
import Discord
import Discord.Types
import Data.Text
import qualified Discord.Requests as R

-- Exposed comamnd key
pongCommand :: Text
pongCommand = "/ping"

pongDesc :: Text
pongDesc = "/ping - Responds with pong \n"
            <> "\tUsage: /ping"

-- Functionality to reply to ping
pongResp :: DiscordHandle -> Event -> IO Bool
pongResp handle (MessageCreate m) = do 
    _ <- restCall handle $  R.CreateMessage (messageChannel m) $ "Pong!"
    pure True