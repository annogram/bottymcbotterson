{-# Language OverloadedStrings #-}
module Events (
    eventPool
    ) where
import PongEvent        (pongCommand
                        , pongResp
                        , pongDesc
                        )
import CovidStatsEvent  (covidStatsCommand
                        , getCovidInfo
                        , covidDesc
                        )
import Data.List
import Discord
import Discord.Types
import qualified Discord.Requests as R
import qualified Data.Map.Lazy as M
import qualified Data.Text as T

-- All the commands that this bot can act on
eventPool :: M.Map T.Text (DiscordHandle -> Event -> IO Bool)
eventPool = M.fromList [ ("/help", helpEvent)
                       , (pongCommand, pongResp)
                       , (covidStatsCommand, getCovidInfo)
                       ]

-- Help commands
helpCommands :: [T.Text]
helpCommands =  [ "/help - Get help message\n" <> "\tUsage: /help"
                , pongDesc
                , covidDesc
                ]

helpEvent :: DiscordHandle -> Event -> IO Bool
helpEvent handle (MessageCreate m) = do
    let text = "Here are the commands you can run: \n" 
                <> "```\n"
                <> (foldl1 (\acc m -> acc <> "\n" <> m) $ helpCommands)
                <> "```"
    _ <- restCall handle $ R.CreateMessage (messageChannel m) $ text
    return True