{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}
module PollEvent 
    ( poll
    ) where
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Text.Regex.TDFA
import Data.List.Split
import System.Random
import qualified Data.Text as T

-- | A Poll consists of a title, a voting category (which is a title and a list of users)
-- | and an id to uniquely identify the poll in memory
data Poll = Poll { title :: T.Text
                 , votes :: [(T.Text, Int)]
                 , pollId :: Int
                 } deriving (Show, Eq)

-- | Exposed comamnd key
pollCommand :: T.Text
pollCommand = "/poll"

-- | Poll description and usage
pollDesc :: T.Text
pollDesc = pollCommand <> " - Responds with pong \n"
            <> "\tUsage: " <> pollCommand

-- | Functionality to create a poll
poll :: T.Text -> IO (Maybe T.Text)
poll t = do
    gen <- newStdGen
    let (n,_) = randomR (0, maxBound :: Int) (gen)
    shared <- atomically $ newTVar $ makePoll n t
    after <- atomically . readTVar $ shared
    return $ Just (T.pack . show $ after)

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