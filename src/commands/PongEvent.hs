{-# Language OverloadedStrings #-}
module PongEvent ( pongEvent)
    where
import Data.Text
import BottyEvent

pongEvent :: BottyEvent
pongEvent = Botty { cmd = pongCommand
                  , desc = pongDesc
                  , func = pongResp
                  }

-- Exposed comamnd key
pongCommand :: Text
pongCommand = "/ping"

pongDesc :: Text -> Text
pongDesc _ = "/ping - Responds with pong \n"
            <> "\tUsage: /ping"

-- Functionality to reply to ping
pongResp :: Text -> IO (Maybe Text)
pongResp _ = return $ Just "Pong!"