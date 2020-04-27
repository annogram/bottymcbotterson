{-# Language OverloadedStrings, TemplateHaskell #-}
module Botty.Commands.CommunityEvent
    ( communityEvent )
    where
import System.Directory (listDirectory, getCurrentDirectory)
import Data.FileEmbed   (getDir, embedDir)
import Data.List
import Data.List.Split
import System.Random
import System.IO
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as TLE
import Botty.Event

communityEvent :: BottyEvent
communityEvent = Botty { cmd = communityCmd
                       , desc = communityDesc
                       , func = communityFunc
                       }

communityFiles :: [(FilePath, B.ByteString)]
communityFiles = $(embedDir "res/community-subtitles")

communityCmd :: T.Text
communityCmd = "/community"

communityDesc :: T.Text -> T.Text
communityDesc _ = communityCmd <> " - get a random quote from community\n"
                <> "\tUsage: " <> communityCmd

communityFunc :: T.Text -> Persistent -> IO (Maybe T.Text)
communityFunc _ _ = pure . Just =<< getRandomQuote

getRandomQuote :: IO T.Text
getRandomQuote = do
    gen <- newStdGen
    srt <- getDir "res/community-subtitles"
    let (fileNo, nextGen) = randomR (0, length srt) (gen)
        (name, contents) = srt !! fileNo
    print $ "Getting a quote from: " <> (fst . break (== '.') $ name)
    quote <- pure . findQuote (nextGen) $ TLE.decodeUtf8 contents
    print quote
    return ("> Quote from: " <> (T.pack . fst . break (== '.') $ name) <> "\n\n"
                <> quote)

findQuote :: StdGen -> T.Text -> T.Text
findQuote gen fileContent = (T.pack .  randomQuote . getQuotes . splitOn ("\r\n\r") . T.unpack) fileContent
    where getQuotes xs = [let (_:_:l) = splitOn ("\r\n") q in unwords l | q <- xs]
          randomQuote xs = let (lineNo, _) = randomR (0, length xs) (gen) in xs !! lineNo
