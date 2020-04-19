{-# Language OverloadedStrings #-}
module Events (
    eventPool
    ) where
import PongEvent        ( pongEvent )
import CovidStatsEvent  ( covidEvent )
import CommunityEvent   ( communityEvent )
import Data.List
import qualified Data.Map.Lazy as M
import qualified Data.Text as T
import BottyEvent

-- | All the commands that this bot can act on
eventPool :: M.Map T.Text (T.Text -> IO (Maybe T.Text))
eventPool = M.fromList $ ("/help", helpEvent):[ (cmd x, func x) | x <- [pongEvent, covidEvent, communityEvent]]

-- | Help commands
helpCommands :: T.Text -> [T.Text]
helpCommands t =  "/help - Get help message\n" <> "\tUsage: /help":[desc x t | x <- [pongEvent, covidEvent, communityEvent]]

-- | Get all help descriptions
helpEvent :: T.Text -> IO (Maybe T.Text)
helpEvent _ = do
    let text = "Here are the commands you can run: \n" 
                <> "```\n"
                <> (foldl1 (\acc m -> acc <> "\n" <> m) $ helpCommands T.empty)
                <> "```"
    return $ Just text