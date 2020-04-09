{-# Language OverloadedStrings #-}
module Events (
    eventPool
    ) where
import Data.List
import Discord
import Discord.Types
import qualified Discord.Requests as R
import PongEvent        (pongCommand, pongResp)
import CovidStatsEvent  (covidStatsCommand, getCovidInfo)
import qualified Data.Map.Lazy as M
import qualified Data.Text as T

-- All the commands that this bot can act on
eventPool :: M.Map T.Text (DiscordHandle -> Event -> IO Bool)
eventPool = M.fromList [ ("/help", helpEvent)
                       , (pongCommand, pongResp)
                    --    , (covidStatsCommand, getCovidInfo)
                       ]

helpEvent :: DiscordHandle -> Event -> IO Bool
helpEvent handle (MessageCreate m) = do
    let text = "Here are the commands you can run: \n" 
                <> (T.unwords $ intersperse ("\n") . map (\x -> "`"  <> x <> "`") $ M.keys eventPool)
    _ <- restCall handle $ R.CreateMessage (messageChannel m) $ text
    return True