{-# Language OverloadedStrings #-}
module Events (
    eventPool
    ) where
import PongEvent        ( pongCommand
                        , pongResp
                        , pongDesc
                        )
import CovidStatsEvent  ( covidStatsCommand
                        , getCovidInfo
                        , covidDesc
                        )
import CommunityEvent   ( communityCmd
                        , communityEvent
                        , communityDesc
                        )
import Data.List
import qualified Data.Map.Lazy as M
import qualified Data.Text as T

-- All the commands that this bot can act on
eventPool :: M.Map T.Text (T.Text -> IO (Maybe T.Text))
eventPool = M.fromList [ ("/help", helpEvent)
                       , (pongCommand, pongResp)
                       , (covidStatsCommand, getCovidInfo)
                       , (communityCmd, communityEvent)
                       ]

-- Help commands
helpCommands :: [T.Text]
helpCommands =  [ "/help - Get help message\n" <> "\tUsage: /help"
                , pongDesc
                , covidDesc
                , communityDesc
                ]

helpEvent :: T.Text -> IO (Maybe T.Text)
helpEvent _ = do
    let text = "Here are the commands you can run: \n" 
                <> "```\n"
                <> (foldl1 (\acc m -> acc <> "\n" <> m) $ helpCommands)
                <> "```"
    return $ Just text