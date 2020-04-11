{-# Language OverloadedStrings #-}
module CovidStatsEvent 
    ( getCovidInfo
    , covidStatsCommand
    , covidDesc
    ) where
import Data.Text        (Text)
import Data.List        (intercalate)
import Data.List.Split  (chunksOf)
import Discord
import Discord.Types
import Data.Aeson.Lens
import Network.Wreq
import Control.Lens
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified Discord.Requests as R
import qualified Data.HashMap.Lazy as M

covidStatsCommand :: Text
covidStatsCommand = "/covid"

covidDesc :: Text
covidDesc = "/covid - Reports statistics on the covid-19 pandameic \n" 
            <> "\tUsage: /covid - all stats \n"
            <> "\tUsage: /covid {country} - countries statistics"

-- Get information on the Covid-19 pandemic
getCovidInfo :: DiscordHandle -> Event -> IO Bool
getCovidInfo handle (MessageCreate m) = do 
    apiData <- covidBasic . T.words . messageText $ m
    case apiData of
        Just (d) -> do
                     _ <- restCall handle $  R.CreateMessage (messageChannel m) $ d
                     return True
        Nothing -> return False


covidBasic :: [Text] -> IO (Maybe Text)
covidBasic (_:[])   = getInfo
covidBasic (_:f)    = getInfoForCountry $ T.unwords f

headerOpt :: Options
headerOpt = defaults & header "Accept" .~ ["application/json"]
                     & checkResponse   .~ (Just $ \_ _ -> return ())

commas :: String -> String
commas = reverse . intercalate "," . chunksOf (3) . reverse . fst . break (== '.')

getInfoForCountry :: Text -> IO (Maybe Text)
getInfoForCountry c = do 
    let url = "https://corona.lmao.ninja/countries/" <> c
    let yestUrl = "https://corona.lmao.ninja/yesterday/" <> c
    -- Get todays information
    r <- getWith headerOpt $ T.unpack url
    let status = r ^. responseStatus . statusCode
    let today = case status of 
                    200 -> Just (r)
                    otherwise -> Nothing

    case today of
        Just (v) -> do
                     y <- getWith headerOpt $ T.unpack yestUrl
                     if y ^. responseStatus . statusCode == 200
                         then do
                                 -- Get countries population
                                let countryCode = v ^. responseBody . key "countryInfo" . key "iso2" . _String
                                p <- getWith headerOpt $ T.unpack $ "https://restcountries.eu/rest/v2/alpha/" <> countryCode
                                let population = p ^?! responseBody . key "population" . _Integer
                                return (Just (craftResponse r y (show population)))
                         else return Nothing
        Nothing  -> return Nothing

getInfo :: IO (Maybe Text)
getInfo = do
    r <- getWith headerOpt "https://corona.lmao.ninja/all"
    let status = r ^. responseStatus . statusCode
    case status of 
        200 -> return (Just (craftBasicResponse r))
        otherwise -> return Nothing

craftResponse :: Response B.ByteString -> Response B.ByteString -> String -> Text
craftResponse r y p = let deaths = r ^?! responseBody . key "deaths" . _Number
                          diffDeaths = deaths - (y ^?! responseBody . key "deaths" . _Number)
                          critical = r ^?! responseBody . key "critical" . _Number
                          diffCritical = critical - (y ^?! responseBody . key "critical" . _Number)
                          todayInfections = r ^?! responseBody . key "todayCases" . _Number
                          diffInfections = todayInfections - (y  ^?! responseBody . key "todayCases" . _Number)
                          totalInfections = r ^?! responseBody . key "cases" . _Double
                          diffTotalInfections = totalInfections - (y ^?! responseBody . key "cases" . _Double)
                          activeInfections = r ^?! responseBody . key "active" . _Number
                          diffActiveInfections = activeInfections - (y ^?! responseBody . key "active" . _Number)
                          recovered = r ^?! responseBody . key "recovered" . _Double
                          percentageRecovered = (recovered / totalInfections) * 100
                    in T.pack (
                        "Countries population: " <> commas p <> "\n"
                        <> ":skull_crossbones: - Deaths :\t" <> (commas . show) deaths
                            <> " (**" <> formatNumber diffDeaths <> "**)" <>"\n"
                        <> ":biohazard: - Critical cases :\t"  <> (commas. show) critical
                            <> " (**" <> formatNumber diffCritical <> "**)" <>"\n"
                        <> ":calendar: - Infections today :\t" <> (commas. show) todayInfections
                            <> " (**" <> formatNumber diffInfections <> "**)" <>"\n"
                        <> ":nauseated_face: - All infections :\t" <> (commas. show) totalInfections
                            <> " (**" <> formatNumber diffTotalInfections <> "**)" <>"\n"
                        <> ":face_vomiting: - Active infections :\t" <> (commas. show) activeInfections
                            <> " (**" <> formatNumber diffActiveInfections <> "**)" <>"\n"
                        <> ":muscle: - Recovered :\t" <> (commas. show) recovered
                            <> " (**" <> (commas . show) percentageRecovered <> "%**)" <>"\n"
                        )
                    where formatNumber n = if (n > 0)
                                            then ("+"++) . commas . show $ n
                                            else ("-"++) . commas . snd . splitAt (1) $ show n

craftBasicResponse :: Response B.ByteString -> Text
craftBasicResponse r = let deaths = commas. show $ r ^?! responseBody . key "deaths" . _Number
                           critical = commas. show $ r ^?! responseBody . key "critical" . _Number
                           todayInfections = commas. show $ r ^?! responseBody . key "todayCases" . _Number
                           totalInfections = commas. show $ r ^?! responseBody . key "cases" . _Number
                           activeInfections = commas. show $ r ^?! responseBody . key "active" . _Number
                           recovered = commas. show $ r ^?! responseBody . key "recovered" . _Number
                   in T.pack (":skull_crossbones: - Deaths :\t" <> deaths <> "\n"
                        <> ":biohazard: - Critical cases :\t"  <> critical <> "\n"
                        <> ":calendar: - Infections today :\t" <> todayInfections <> "\n"
                        <> ":nauseated_face: - All infections :\t" <> totalInfections <> "\n"
                        <> ":face_vomiting: - Active infections :\t" <> activeInfections <> "\n"
                        <> ":muscle: - Recovered :\t" <> recovered <> "\n")