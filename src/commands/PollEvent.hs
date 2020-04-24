{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}
module PollEvent 
    ( pollEvent
    , makePoll
    ) where
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Text.Regex.TDFA
import Data.List.Split
import System.Random
import BottyEvent
import qualified Data.Map.Lazy as M
import qualified Data.Text as T

-- | A Poll consists of a title, a voting category (which is a title and a list of users)
-- and an id to uniquely identify the poll in memory
data Poll = Poll { title :: T.Text
                 , votes :: [(T.Text, Int)]
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
pollDesc _ = pollCommand <> " - Responds with pong \n"
            <> "\tUsage: " <> pollCommand

-- | Functionality to create a poll
poll :: T.Text -> Persistent -> IO (Maybe T.Text)
poll t p = do
    gen <- newStdGen
    let (n,_) = randomR (0, maxBound :: Int) (gen)
        poll = makePoll n t
    writeStore p poll
    temp <- readTVarIO p
    return $ Just (T.pack . show $ poll)

writeStore :: Persistent -> Poll -> IO ()
writeStore st p = atomically $ readTVar st >>= 
    \store -> writeTVar st $ M.insert (pollId p) (show p) store

-- | Functionality to cast a vote

-- | Parse a string into a poll
makePoll :: Int -> T.Text  -> Poll
makePoll pollId t = let votes = [ (x, 0) | x <- getCategories t]
                        title = getTitle t
                    in Poll {votes , pollId, title}

-- | Regular expression matching
getCategories :: T.Text -> [T.Text]
getCategories t' = let (_,_,_,xs) =  t' =~ (".*{([a-z, ]+)}" :: T.Text) :: (T.Text, T.Text, T.Text, [T.Text])
                   in map (T.filter (/=' ')) . T.splitOn "," . head $ xs

getTitle :: T.Text -> T.Text
getTitle t = let (interest,_) = T.breakOn "{" t
             in T.unwords . tail . T.words $ interest