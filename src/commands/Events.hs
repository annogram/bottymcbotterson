{-# Language OverloadedStrings #-}
module Events ( eventPool
              ) where
import PongEvent        ( pongEvent )
import CovidStatsEvent  ( covidEvent )
import CommunityEvent   ( communityEvent )
import PollEvent        ( pollEvent )
import Data.List
import Botty.Event
import qualified Data.Map.Lazy as M
import qualified Data.Text as T

events :: [BottyEvent]
events = [pongEvent, covidEvent, communityEvent, pollEvent]

followups :: [BottyFollowUp]
followups = []

-- | All the commands that this bot can act on
eventPool :: M.Map T.Text (T.Text -> Persistent -> IO (Maybe T.Text))
eventPool = M.fromList $ ("/help", helpEvent):[ (cmd x, func x) | x <- events ]

followUpPool :: M.Map T.Text (T.Text -> Persistent -> IO (Maybe T.Text))
followUpPool = M.fromList $ [ (fcmd x, ffunc x) | x <- followups ]

-- | Help commands
helpCommands :: T.Text -> [T.Text]
helpCommands t =  "/help - Get help message\n" <> "\tUsage: /help":[desc x t | x <- events]

-- | Get all help descriptions
helpEvent :: T.Text -> Persistent -> IO (Maybe T.Text)
helpEvent _ _ = do
    let text = "Here are the commands you can run: \n" 
                <> "```\n"
                <> (foldl1 (\acc m -> acc <> "\n" <> m) $ helpCommands T.empty)
                <> "```"
    return $ Just text