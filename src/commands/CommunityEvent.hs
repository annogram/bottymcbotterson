{-# Language OverloadedStrings #-}
{-# Language NamedFieldPuns #-}
module CommunityEvent 
    ( communityCmd
    , communityEvent
    , communityDesc
    , getRandomQuote
    ) where
import Paths_discord_bot    (getDataDir)
import System.Directory     (listDirectory)
import qualified Data.Text as T
import Data.List
import Data.List.Split
import System.Random
import System.IO
import Discord
import Discord.Types
import qualified Discord.Requests as R

communityCmd :: T.Text
communityCmd = "/community"

communityDesc :: T.Text
communityDesc = communityCmd <> " - get a random quote from community\n"
                <> "\tUsage: " <> communityCmd

communityEvent :: DiscordHandle -> Event -> IO Bool
communityEvent handle (MessageCreate m) = do
    randomQuote <- getRandomQuote
    _ <- restCall handle $ R.CreateMessage (messageChannel m) $ randomQuote
    pure True

getRandomQuote :: IO T.Text
getRandomQuote = do
    gen <- newStdGen
    cwd <- getDataDir
    print cwd
    let baseDir = cwd <> "\\res\\community-subtitles"
    x   <- listDirectory $ baseDir
    let (fileNo, nextGen) = randomR (0, length x) (gen)
        fileName = x !! fileNo
        file = baseDir <> "\\" <> fileName
    print $ "Getting a quote from: " <> (fst . break (== '.') $ fileName)
    quote <- pure . findQuote (nextGen) =<< readFile file
    return ("> Quote from: " <> (T.pack . fst . break (== '.') $ fileName) <> "\n\n"
                <> quote)

findQuote :: StdGen -> String -> T.Text
findQuote gen fileContent = (T.pack . randomLine . getQuote . splitOn ("\n\n")) fileContent
    where getQuote xs = [let (_:_:l) = lines q in unwords l | q <- xs]
          randomLine xs = let (lineNo, _) = randomR (0, length xs) (gen) in xs !! lineNo
          