{-# Language OverloadedStrings #-}
module CommunityEvent 
    ( communityCmd
    , communityEvent
    , communityDesc
    , getRandomQuote
    ) where
import qualified Data.Text as T
import Data.List
import System.Random
import System.IO
import System.Directory
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
    gen <- getStdGen
    cwd <- getCurrentDirectory
    x   <- getDirectoryContents $ cwd <> "\\res\\community-subtitles"
    let (fileNo, nextGen) = randomR (0, length x) (gen)
    let file = x !! fileNo
    pure (T.pack file)
