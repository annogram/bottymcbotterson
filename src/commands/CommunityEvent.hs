{-# Language OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module CommunityEvent 
    ( communityCmd
    , communityEvent
    , communityDesc
    , getRandomQuote
    , communityFiles
    ) where
import System.Directory (listDirectory, getCurrentDirectory)
import Data.FileEmbed   (getDir, embedDir)
import qualified Data.Text as T
import Data.List
import Data.List.Split
import System.Random
import System.IO
import Discord
import Discord.Types
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as TLE
import qualified Discord.Requests as R

communityFiles :: [(FilePath, B.ByteString)]
communityFiles = $(embedDir "res/community-subtitles")

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
    srt <- getDir "res/community-subtitles"
    let (fileNo, nextGen) = randomR (0, length srt) (gen)
        (name, contents) = srt !! fileNo
    print $ "Getting a quote from: " <> (fst . break (== '.') $ name)
    quote <- pure . findQuote (nextGen) $ TLE.decodeUtf8 contents
    print quote
    -- let t = splitOn ("\r\n\r") . T.unpack $ TLE.decodeUtf8 contents
    -- print t
    -- print quote
    return ("> Quote from: " <> (T.pack . fst . break (== '.') $ name) <> "\n\n"
                <> quote)
    -- pure ""

findQuote :: StdGen -> T.Text -> T.Text
findQuote gen fileContent = (T.pack .  randomQuote . getQuotes . splitOn ("\r\n\r") . T.unpack) fileContent
    where getQuotes xs = [let (_:_:l) = splitOn ("\r\n") q in unwords l | q <- xs]
          randomQuote xs = let (lineNo, _) = randomR (0, length xs) (gen) in xs !! lineNo
