{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}
module PollEvent 
    ( pollEvent
    , emojiRange
    ) where
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Text.Regex.TDFA
import Data.List.Split
import System.Random
import BottyEvent
import Data.List
import Numeric
import Data.Char
import Text.Emoji
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
        (pollCommand:t') = T.unpack t
        poll = makePoll n $ T.pack t'
    case poll of
        Nothing -> return (Just $ pollDesc T.empty)
        Just (poll') -> do 
            writeStore p poll'
            temp <- readTVarIO p
            return $ Just (T.pack . show $ poll')

writeStore :: Persistent -> Poll -> IO ()
writeStore st p = atomically $ readTVar st >>= 
    \store -> writeTVar st $ M.insert (pollId p) (show p) store

-- | Functionality to cast a vote

-- | Parse a string into a poll
makePoll :: Int -> T.Text  -> Maybe Poll
makePoll pollId t = case parseInformation t of
                        Nothing -> Nothing
                        Just (title, categories) -> Just Poll {votes = [ (x, 0) | x <- categories] , pollId, title}

-- | Regular expression matching
parseInformation :: T.Text -> Maybe (T.Text, [T.Text])
parseInformation t' = let (a,_,_,xs) =  t' =~ ("\\(([a-z, ]+)\\)" :: T.Text) :: (T.Text, T.Text, T.Text, [T.Text])
                   in case xs of
                       [] -> Nothing
                       (xs') -> Just (a, nub . map (T.filter (/=' ')) . T.splitOn "," . head $ xs')

emojiRange :: [T.Text]
emojiRange = let ranges = (\(a,b) -> [readH a .. readH b]) <$> [ ("1F300", "1F320")
                          , ("1F330", "1F335")
                          , ("1F337", "1F37C")
                          , ("1F380", "1F393")
                          , ("1F3A0", "1F3C4")
                          , ("1F3C6", "1F3CA")
                          , ("1F3E0", "1F3F0")
                          , ("1F400", "1F43E")
                          , ("1F442", "1F4F7")
                          ] 
             in (discordSyn . T.singleton . chr) <$> concat ranges
    where readH c = let (n,_):[] = readHex c
                    in n
          discordSyn x = let Just (e:_) = aliasesFromEmoji x
                         in e