{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}
module PollEvent 
    ( pollEvent
    ) where
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Text.Regex.TDFA
import Data.List.Split
import System.Random
import Botty.Event
import Data.List
import Botty.Utils
import Text.Emoji
import qualified Data.Map.Lazy as M
import qualified Data.Text as T

-- | A Poll consists of a title, a voting category (which is a title and a list of users)
-- and an id to uniquely identify the poll in memory
data Poll = Poll { title :: T.Text
                 , votes :: [(T.Text, Int, T.Text)]
                 , pollId :: Int
                 } deriving (Read, Show, Eq)

pollEvent :: BottyEvent
pollEvent = Botty { cmd = pollCommand
                  , desc = pollDesc
                  , func = poll
                  }

-- | Exposed comamnd key
pollCommand :: T.Text
pollCommand = "/poll"

-- | Poll description and usage
pollDesc :: T.Text -> T.Text
pollDesc _ = pollCommand <> " - Starts a poll with options \n"
            <> "\tUsage: " <> pollCommand

-- | Functionality to create a poll
poll :: T.Text -> Persistent -> IO (Maybe T.Text)
poll t p = do
    gen <- newStdGen
    let (n,newGen) = randomR (0, maxBound :: Int) (gen)
        t' =  (T.unwords . tail . T.words) t
    poll <- makePoll n t'
    case poll of
        Nothing -> return (Just $ pollDesc T.empty)
        Just (poll') -> do 
            writeStore p poll'
            temp <- readTVarIO p
            return $ Just (printPoll poll')

writeStore :: Persistent -> Poll -> IO ()
writeStore st p = atomically $ readTVar st >>= 
    \store -> writeTVar st $ M.insert (pollId p) (show p) store

-- | Functionality to cast a vote

-- | Parse a string into a poll
makePoll :: Int -> T.Text -> IO (Maybe Poll)
makePoll pollId t = do
    case parseInformation t of
        Nothing -> pure Nothing
        Just (title, categories) -> do
            e <- forM categories (\_ -> randomEmoji)
            let dogegories = zip categories e
            return $ Just Poll {votes = [ (x, 0, y) | (x,y) <- dogegories] , pollId, title}


-- | Regular expression matching
parseInformation :: T.Text -> Maybe (T.Text, [T.Text])
parseInformation t' = let (a,_,_,xs) =  t' =~ ("\\((.+)\\)" :: T.Text) :: (T.Text, T.Text, T.Text, [T.Text])
                   in case xs of
                       [] -> Nothing
                       (xs') -> Just (a, nub . map (T.filter (/=' ')) . T.splitOn "," . head $ xs')

-- | Print the poll as a message to send to discord
printPoll :: Poll -> T.Text
printPoll p = let totalVotes = sum . map (\(_,v,_) -> v) $ votes p
    in "> " <> title p <> "\n"
                <> "_ poll id: " <> (T.pack . show . pollId) p <> "_" <> "\n"
                <> votesStr totalVotes <> "\n"
    where votesStr to 
            | to == 0 = T.concat $ map (\(t,v,e) -> ":" <> e <> ":" <> " - " <> t <> ": 0%\n") (votes p)
            | otherwise = T.concat $ map (\(t,v,e) -> e <> " - " <> t <> ": " <> (T.pack . show) (v `doDiv` to) <> "%") (votes p)