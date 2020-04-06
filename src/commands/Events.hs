{-# Language OverloadedStrings #-}
module Events (
    eventPool
    ) where
import Discord
import Discord.Types
import qualified Discord.Requests as R
import qualified Data.Map.Lazy as M
import Data.Text
import PongEvent (pongCommand, pongResp)

eventPool :: M.Map Text (DiscordHandle -> Event -> IO ())
eventPool = M.fromList [ (pongCommand, pongResp) ]