{-# Language OverloadedStrings, DeriveGeneric #-}
module CovidStatsEvent 
    ( getCovidInfo
    , covidStatsCommand
    , covidDesc
    , getInfo
    ) where
import Data.Text        (Text)
import Data.List        (intercalate)
import Data.List.Split  (chunksOf)
import Data.Aeson       (FromJSON, ToJSON)
import Network.HTTP.Req
import GHC.Generics
import qualified Data.Text as T
import Control.Exception
import qualified GlobalStats as G
import qualified CountryStats as C

covidStatsCommand :: Text
covidStatsCommand = "/covid"

covidDesc :: Text
covidDesc = "/covid - Reports statistics on the covid-19 pandameic \n" 
            <> "\tUsage: /covid - all stats \n"
            <> "\tUsage: /covid {country} - countries statistics"

-- Get information on the Covid-19 pandemic
getCovidInfo :: Text -> IO (Maybe Text)
getCovidInfo text = return =<< covidBasic . T.words $ text


covidBasic :: [Text] -> IO (Maybe Text)
covidBasic (_:[])   = getInfo
covidBasic (_:f)    = getInfoForCountry $ T.unwords f

commas :: String -> String
commas = reverse . intercalate "," . chunksOf (3) . reverse . fst . break (== '.')

httpConfig = defaultHttpConfig {httpConfigCheckResponse = noNoise}
    where noNoise _ _ _ = Nothing


getInfo :: IO (Maybe Text)
getInfo = do
    let url = https "corona.lmao.ninja" /: "all"
    meme <- catch (runReq httpConfig $ do
                            r' <- req GET url NoReqBody
                                    jsonResponse mempty
                            return $ Just (responseBody r' :: G.GlobalStat))
                    (\(JsonHttpException e) -> do
                        print e
                        return Nothing)
    case meme of
        Nothing -> return Nothing
        Just (i) -> return (Just $ craftBasicResponse i)

getInfoForCountry :: Text -> IO (Maybe Text)
getInfoForCountry c = do
    let url = https "corona.lmao.ninja" /: "countries" /: c
        yestUrl = https "corona.lmao.ninja" /: "yesterday" /: c
        restContries = https "restcountries.eu" /: "rest" /: "v2" /: "alpha"
    today <- catch
                (runReq httpConfig $ pure =<< constructSubMonad url)
                (\(JsonHttpException _) -> return Nothing)
    yesterday <- catch
                    (runReq httpConfig $ pure =<< constructSubMonad yestUrl)
                    (\(JsonHttpException _) -> return Nothing)
    case today of
        Nothing     -> return Nothing
        Just (info) -> do
            let restCountries' = restContries /: (C.iso2 . C.countryInfo $ info)
                Just (yestInfo) = yesterday
            r <- runReq httpConfig $ do
                    req GET
                        restCountries'
                        NoReqBody
                        jsonResponse
                        mempty
            let b = responseBody r
            return $ Just (craftResponse info yestInfo b)
    where constructSubMonad u = do
            r' <- req GET
                u
                NoReqBody
                jsonResponse
                mempty
            return (Just (responseBody r' ))

craftBasicResponse :: G.GlobalStat -> Text
craftBasicResponse r = let deaths' = commas . show $ G.deaths r
                           critical' = commas. show $ G.critical r
                           todayInfections = commas . show $ G.todayCases r
                           totalInfections = commas . show $ G.cases r
                           activeInfections = commas . show $ G.active r
                           recovered' = commas. show $ G.recovered r
                   in T.pack (":skull_crossbones: - Deaths :\t" <> deaths' <> "\n"
                        <> ":biohazard: - Critical cases :\t"  <> critical' <> "\n"
                        <> ":calendar: - Infections today :\t" <> todayInfections <> "\n"
                        <> ":nauseated_face: - All infections :\t" <> totalInfections <> "\n"
                        <> ":face_vomiting: - Active infections :\t" <> activeInfections <> "\n"
                        <> ":muscle: - Recovered :\t" <> recovered' <> "\n")

craftResponse :: C.CountryStat -> C.CountryStat -> C.Population  -> Text
craftResponse r y (C.Population p) =  let deaths' = C.deaths r
                                          diffDeaths = deaths' - C.deaths y
                                          critical' = C.critical r
                                          diffCritical = critical' - C.critical y
                                          todayInfections = C.todayCases r
                                          diffInfections = todayInfections - C.todayCases y
                                          totalInfections = C.cases r
                                          diffTotalInfections = totalInfections - C.cases y
                                          activeInfections = C.active r
                                          diffActiveInfections = activeInfections - C.active y
                                          recovered' = C.recovered r
                                          percentageRecovered = (recovered' `div` totalInfections) * 100
                                      in T.pack (
                                            (T.unpack . C.country) r <> "'s population: " <> (commas . show) p <> "\n"
                                            <> ":skull_crossbones: - Deaths :\t" <> (commas . show) deaths'
                                                <> " (**" <> formatNumber diffDeaths <> "**)" <>"\n"
                                            <> ":biohazard: - Critical cases :\t"  <> (commas. show) critical'
                                                <> " (**" <> formatNumber diffCritical <> "**)" <>"\n"
                                            <> ":calendar: - Infections today :\t" <> (commas. show) todayInfections
                                                <> " (**" <> formatNumber diffInfections <> "**)" <>"\n"
                                            <> ":nauseated_face: - All infections :\t" <> (commas. show) totalInfections
                                                <> " (**" <> formatNumber diffTotalInfections <> "**)" <>"\n"
                                            <> ":face_vomiting: - Active infections :\t" <> (commas. show) activeInfections
                                                <> " (**" <> formatNumber diffActiveInfections <> "**)" <>"\n"
                                            <> ":muscle: - Recovered :\t" <> (commas. show) recovered'
                                                <> " (**" <> (commas . show) percentageRecovered <> "%**)" <>"\n"
                                          )
                    where formatNumber n = if (n >= 0)
                                            then ("+"++) . commas . show $ n
                                            else ("-"++) . commas . snd . splitAt (1) $ show n

-- getInfoForCountry2 :: Text -> IO (Maybe Text)
-- getInfoForCountry2 c = do 
--     let url = "https://corona.lmao.ninja/countries/" <> c
--         yestUrl = "https://corona.lmao.ninja/yesterday/" <> c
--     -- Get todays information
--     r <- getWith headerOpt $ T.unpack url
--     let status = r ^. responseStatus . statusCode
--         today = case status of 
--                     200 -> Just (r)
--                     otherwise -> Nothing

--     case today of
--         Just (v) -> do
--             y <- getWith headerOpt $ T.unpack yestUrl
--             if y ^. responseStatus . statusCode == 200
--                 then do
--                     -- Get countries population
--                     let countryCode = v ^. responseBody . key "countryInfo" . key "iso2" . _String
--                     p <- getWith headerOpt $ T.unpack $ "https://restcountries.eu/rest/v2/alpha/" <> countryCode
--                     let population = p ^?! responseBody . key "population" . _Integer
--                     return (Just (craftResponse r y (show population)))
--                 else return Nothing
--         Nothing  -> return Nothing

-- getInfo :: IO (Maybe Text)
-- getInfo = do
--     r <- getWith headerOpt "https://corona.lmao.ninja/all"
--     let status = r ^. responseStatus . statusCode
--     case status of 
--         200 -> return $ Just (craftBasicResponse r)
--         otherwise -> return Nothing




