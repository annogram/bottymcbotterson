{-# Language OverloadedStrings #-}
module CovidStatsEvent 
    ( getCovidInfo
    , covidStatsCommand
    ) where
import Discord
import Discord.Types
import Data.Text
import Data.Aeson.Lens
import Network.Wreq
import Control.Lens
import qualified Discord.Requests as R

covidStatsCommand :: Text
covidStatsCommand = "/covid"

-- Get information on the Covid-19 pandemic
getCovidInfo :: DiscordHandle -> Event -> IO ()
getCovidInfo handle (MessageCreate m) = do 
    ip <- getInfo
    _ <- restCall handle $  R.CreateMessage (messageChannel m) $ ip
    return ()

headerOpt :: Options
headerOpt = defaults & header "Accept" .~ ["application/json"]

getInfo :: IO (Text)
getInfo = do 
    r <- getWith headerOpt "http://httpbin.org/get"
    let [v] = r ^.. responseBody . key "origin" . _String
    return (v)