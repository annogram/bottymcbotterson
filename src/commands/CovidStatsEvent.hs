{-# Language OverloadedStrings #-}
module CovidStatsEvent 
    ( getCovidInfo
    , covidStatsCommand
    ) where
import Data.Text (Text)
import Data.List (intercalate)
import Data.List.Split (chunksOf)
import Discord
import Discord.Types
import Data.Aeson.Lens
import Network.Wreq
import Control.Lens
import Data.Map as Map
import qualified Data.Text as T
import qualified Discord.Requests as R
import qualified Data.HashMap.Lazy as M

covidStatsCommand :: Text
covidStatsCommand = "/covid"

-- Get information on the Covid-19 pandemic
getCovidInfo :: DiscordHandle -> Event -> IO ()
getCovidInfo handle (MessageCreate m) = do 
    apiData <- covidBasic . T.words . messageText $ m
    _ <- restCall handle $  R.CreateMessage (messageChannel m) $ apiData
    return ()

covidBasic :: [Text] -> IO (Text)
covidBasic ("/covid":[])   = getInfo
covidBasic ("/covid":f)  = getInfoForCountry $ T.unwords f

headerOpt :: Options
headerOpt = defaults & header "Accept" .~ ["application/json"]

commas :: [Char] -> [Char]
commas = reverse . intercalate "," . chunksOf (3) . reverse . fst . break (== '.')

getInfoForCountry :: Text -> IO (Text)
getInfoForCountry c = do 
    let url = "https://corona.lmao.ninja/countries/" <> c
    r <- getWith headerOpt $ T.unpack url
    let deaths = commas. show $ r ^?! responseBody . key "deaths" . _Number
    let critical = commas. show $ r ^?! responseBody . key "critical" . _Number
    let todayInfections = commas. show $ r ^?! responseBody . key "todayCases" . _Number
    let totalInfections = commas. show $ r ^?! responseBody . key "cases" . _Number
    let activeInfections = commas. show $ r ^?! responseBody . key "active" . _Number
    let recovered = commas. show $ r ^?! responseBody . key "recovered" . _Number
    let info = ":skull_crossbones: - Deaths :\t" <> deaths <> "\n"
                    <> ":biohazard: - Critical cases :\t"  <> critical <> "\n"
                    <> ":calendar: - Infections today :\t" <> todayInfections <> "\n"
                    <> ":nauseated_face: - All infections :\t" <> totalInfections <> "\n"
                    <> ":face_vomiting: - Active infections :\t" <> activeInfections <> "\n"
                    <> ":muscle: - Recovered :\t" <> recovered <> "\n"

    return (T.pack info)

getInfo :: IO (Text)
getInfo = do
    r <- getWith headerOpt "https://corona.lmao.ninja/all"
    let deaths = commas . show $ r ^?! responseBody . key "deaths" . _Number
    let critical = commas. show $ r ^?! responseBody . key "critical" . _Number
    let todayInfections = commas. show $ r ^?! responseBody . key "todayCases" . _Number
    let totalInfections = commas. show $ r ^?! responseBody . key "cases" . _Number
    let activeInfections = commas. show $ r ^?! responseBody . key "active" . _Number
    let recovered = commas. show $ r ^?! responseBody . key "recovered" . _Number
    let info = ":skull_crossbones: - Deaths :\t " <> deaths <> "\n"
                    <> ":biohazard: - Critical cases :\t"  <> critical <> "\n"
                    <> ":calendar: - Infections today :\t" <> todayInfections <> "\n"
                    <> ":nauseated_face: - All infections :\t" <> totalInfections <> "\n"
                    <> ":face_vomiting: - Active infections :\t" <> activeInfections <> "\n"
                    <> ":muscle: - Recovered :\t" <> recovered <> "\n"
    return (T.pack info)
