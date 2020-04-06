{-# Language OverloadedStrings #-}
module PongEvent
    ( pongResp
    , pongCommand
    ) where
import Control.Monad        (when)
import System.Environment   (getEnv)
import Discord
import Discord.Types
import Data.Text
import qualified Discord.Requests as R

pongCommand :: Text
pongCommand = "/ping"

pongResp :: DiscordHandle -> Event -> IO ()
pongResp handle (MessageCreate m) = do 
    _ <- restCall handle $  R.CreateMessage (messageChannel m) $ "Pong!"
    pure ()