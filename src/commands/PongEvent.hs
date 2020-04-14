{-# Language OverloadedStrings #-}
module PongEvent
    ( pongResp
    , pongCommand
    , pongDesc
    ) where
import Data.Text

-- Exposed comamnd key
pongCommand :: Text
pongCommand = "/ping"

pongDesc :: Text
pongDesc = "/ping - Responds with pong \n"
            <> "\tUsage: /ping"

-- Functionality to reply to ping
pongResp :: Text -> IO (Maybe Text)
pongResp _ = return $ Just "Pong!"