{-# Language OverloadedStrings #-}
module CovidStatsEvent 
    ( getCovidInfo
    , covidStatsCommand
    , getInfoForCountry
    ) where
import Data.Text (Text)
import Discord
import Discord.Types
import Data.Aeson.Lens
import Network.Wreq
import Control.Lens
import qualified Data.Text as T
import qualified Discord.Requests as R
import qualified Data.HashMap.Lazy as M

covidStatsCommand :: Text
covidStatsCommand = "/covid"

-- Get information on the Covid-19 pandemic
getCovidInfo :: DiscordHandle -> Event -> IO ()
getCovidInfo handle (MessageCreate m) = do 
    apiData <- getInfoForCountry . messageText $ m
    _ <- restCall handle $  R.CreateMessage (messageChannel m) $ apiData
    return ()

covidBasic :: [Text] -> Text
covidBasic (["/covid", f]) = "test"
covidBasic (["/covid"]) = "test 2"

headerOpt :: Options
headerOpt = defaults & header "Accept" .~ ["application/json"]

getInfoForCountry :: Text -> IO (Text)
getInfoForCountry c = do 
    let url = "https://corona.lmao.ninja/countries/" <> c
    r <- getWith headerOpt $ T.unpack url
    let ans = r ^.. responseBody
    print ans
    return ("ho")

getInfo :: IO (Text)
getInfo = do
    r <- getWith headerOpt "https://corona.lmao.ninja/all"
    let ans = r ^.. responseBody
    print ans
    return ("ho")
    