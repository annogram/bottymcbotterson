{-# LANGUAGE OverloadedStrings, NamedFieldPuns, DeriveGeneric #-}
module Botty.Commands.PollEvent 
    ( pollEvent
    , pollFollowUp
    , vote
    , unvote
    ) where
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Text.Regex.TDFA
import Data.List.Split
import Data.Char
import Data.Serialize
import System.Random
import Botty.Event
import Data.List
import Botty.Utils
import Text.Emoji
import Text.Printf
import Discord
import GHC.Generics
import Discord.Types
import qualified Discord.Requests as R
import qualified Data.Map.Lazy as M
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE

type Vote = (T.Text, T.Text, [UserId])

-- | A Poll consists of a title, a voting category (which is a title and a list of users)
-- and an id to uniquely identify the poll in memory
data Poll = Poll { title :: T.Text
                 , votes :: [Vote]
                 , pollId :: Int
                 } deriving (Eq, Generic)

instance Serialize T.Text where
    put = put . TE.encodeUtf8
    get = TE.decodeUtf8 <$> get

instance Serialize Snowflake where
    put = put . show
    get = read <$> get

instance Serialize Poll

pollEvent :: BottyEvent
pollEvent = Botty { cmd = pollCommand
                  , desc = pollDesc
                  , func = poll
                  }

pollFollowUp :: BottyFollowUp
pollFollowUp = Follow { fcmd = pollCommand
                      , ffunc = followUp
                      }

type RegCap = (T.Text, T.Text, T.Text, [T.Text])

-- | Exposed comamnd key
pollCommand :: T.Text
pollCommand = "/poll"

-- | Poll description and usage
pollDesc :: T.Text -> T.Text
pollDesc _ = pollCommand <> " - Starts a poll with options \n"
            <> "\tUsage: " <> pollCommand <> " Poll title (poll options,other option)\n"
            <> "\tvoting: click on the emoji that relates to the option in the poll"

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
    \store -> writeTVar st $ M.insert (pollId p) (encode p) store

-- | Parse a string into a poll
makePoll :: Int -> T.Text -> IO (Maybe Poll)
makePoll pollId t = do
    case parseInformation t of
        Nothing -> pure Nothing
        Just (title, categories) -> do
            e <- forM categories (\_ -> randomEmoji)
            let dogegories = zip categories e
            return $ Just Poll {votes = [ (x, y, []) | (x,y) <- dogegories] , pollId, title}


-- | Regular expression matching
parseInformation :: T.Text -> Maybe (T.Text, [T.Text])
parseInformation t' = let (a,_,_,xs) =  t' =~ ("\\((.+)\\)" :: T.Text) :: RegCap
                   in case xs of
                       [] -> Nothing
                       (xs') -> Just (a, nub . map (T.strip) . T.splitOn "," . head $ xs')

-- | Print the poll as a message to send to discord
printPoll :: Poll -> T.Text
printPoll p = let totalVotes = sum . map (\(_,_,vs) -> length vs) $ votes p
    in "> " <> title p <> "\n"
                <> "_ poll id: " <> (T.pack . show . pollId) p <> "_" <> "\n"
                <> votesStr totalVotes <> "\n"
    where votesStr to 
            | to == 0 = T.concat $ map (\(t,e,_) -> ":" <> e <> ":" <> " - " <> t <> ": 0%\n") (votes p)
            | otherwise = T.concat $ map 
                (\(t,e,us) -> ":" <> e <> ":" <> " - " <> t <> ": " 
                    <> (T.pack . fst . break (== '.') . show) (100 * (length us) `doDiv` to) <> 
                    if length us == 0 
                        then "%\n"
                        else "% (" <> voters us <> ")\n")
                (votes p)
          voters [] = T.empty
          voters (x:[]) = "<@" <> (T.pack . show) x <> ">" <> voters []
          voters (x:xs) = "<@" <> (T.pack . show) x <> ">, " <> voters xs

-- | This follow up will add the reactions to message after it's been posted
followUp ::  DiscordHandle -> Message -> T.Text -> Persistent -> IO (Maybe T.Text)
followUp h m t p = do
    persistent <- readTVarIO p
    let poll = pollFromMessage (messageText m) persistent
    case poll of
        Nothing -> return Nothing
        Just (poll') -> do
            let es = [ e | (_,e,_) <- votes $ poll']
            forM_ es (\e -> restCall h $ R.CreateReaction (messageChannel m, messageId m) e)
            return Nothing

-- | Construct a poll from a message (with the poll id) and persistent storage
pollFromMessage :: T.Text -> M.Map Int BS.ByteString -> Maybe Poll
pollFromMessage m p = let (_,_,_,pid:_) = m =~ ("poll id: ([0-9]+)" :: T.Text) :: RegCap
                    in case decode <$> M.lookup (read . T.unpack $ pid :: Int) p of
                        Nothing -> Nothing
                        Just (Left _) -> Nothing
                        Just (Right (poll)) -> Just poll

-- | Common functionality for voting and unvoting
cast :: (Vote -> Vote) -> Poll -> Persistent -> STM ()
cast fn p store = do
    let votes' = reverse . sortBy (sortGT) . map (fn) $ votes p
        newPoll = Poll {votes = votes', pollId = pollId p, title = title p}
    persistent <- readTVar store
    writeTVar store $ M.insert (pollId newPoll) (encode newPoll) persistent
    where sortGT (_,_,vs) (_,_,us) = compare (length vs) (length us)

-- | Functionality to cast a vote
vote :: DiscordHandle -> ReactionInfo -> Persistent -> IO (Maybe T.Text)
vote h ri p = do
    let e = reactionEmoji ri
    Right (m) <- restCall h $ R.GetChannelMessage (reactionChannelId ri, reactionMessageId ri)
    Right (user) <- restCall h $ R.GetUser (reactionUserId ri)
    persistent <- readTVarIO p
    case pollFromMessage (messageText m) persistent of
        Just oldPoll -> do
            print $ "User: " ++ (T.unpack . userName) user ++ " cast a vote on a poll"
            atomically $ cast (\(x,em,us) -> if em == (discordSyn . emojiName) e
                    then (x,em,(reactionUserId ri):us)
                    else (x,em,us)
                    ) oldPoll p
            newPersistent <- readTVarIO p
            let Just newPoll = pollFromMessage (messageText m) newPersistent
            _ <- restCall h $ R.EditMessage (messageChannel m, messageId m) (printPoll newPoll) Nothing
            return Nothing
        Nothing -> return Nothing
    

-- | When users undo a selection
unvote :: DiscordHandle -> ReactionInfo -> Persistent -> IO (Maybe T.Text)
unvote h ri p = do
    let e = reactionEmoji ri
    Right (m) <- restCall h $ R.GetChannelMessage (reactionChannelId ri, reactionMessageId ri)
    Right (user) <- restCall h $ R.GetUser (reactionUserId ri)
    persistent <- readTVarIO p
    case pollFromMessage (messageText m) persistent of
        Nothing -> return Nothing
        Just oldPoll -> do
            print $ "User: " ++ (T.unpack . userName) user ++ " uncast a vote on a poll"
            atomically $ cast (\(x,em,us) -> if em == (discordSyn . emojiName) e 
                    then (x, em, delete (reactionUserId ri) us)
                    else (x, em, us)
                    ) oldPoll p
            newPersistent <- readTVarIO p
            let Just newPoll = pollFromMessage (messageText m) newPersistent
            _ <- restCall h $ R.EditMessage (messageChannel m, messageId m) (printPoll newPoll) Nothing
            return Nothing