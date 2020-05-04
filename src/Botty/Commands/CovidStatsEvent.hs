{-# Language OverloadedStrings, ScopedTypeVariables #-}
module Botty.Commands.CovidStatsEvent 
    (covidEvent)
    where
import Data.Text         (Text)
import Data.List         (intercalate)
import Data.List.Split   (chunksOf)
import Data.Function     (on)
import Control.Exception (catch)
import Network.HTTP.Req
import Text.Printf
import qualified Data.Text as T
import qualified Botty.Commands.Types.GlobalStats as G
import qualified Botty.Commands.Types.CountryStats as C
import Botty.Event
import Botty.Utils (doDiv)

covidEvent :: BottyEvent
covidEvent = Botty { cmd = covidStatsCommand 
                   , desc = covidDesc
                   , func = getCovidInfo}

covidStatsCommand :: Text
covidStatsCommand = "/covid"

covidDesc :: Text -> Text
covidDesc _ = "/covid - Reports statistics on the covid-19 pandameic \n" 
                <> "\tUsage: /covid - all stats \n"
                <> "\tUsage: /covid {country} - countries statistics \n"
                <> "\tUsage: /covid {country,country,...} - countries statistics"

-- Get information on the Covid-19 pandemic
getCovidInfo :: Text -> Persistent -> IO (Maybe Text)
getCovidInfo text _ = return =<< covidBasic . T.words $ text


covidBasic :: [Text] -> IO (Maybe Text)
covidBasic (_:[])   = getInfo
covidBasic (_:f:_)    = getInfoForCountry $ T.splitOn "," f

commas :: String -> String
commas = reverse . intercalate "," . chunksOf (3) . reverse . fst . break (== '.')

httpConfig = defaultHttpConfig {httpConfigCheckResponse = noNoise}
    where noNoise _ _ _ = Nothing

getInfo :: IO (Maybe Text)
getInfo = do
    let url = https "corona.lmao.ninja" /: "v2" /: "all"
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

getInfoForCountry :: [Text] -> IO (Maybe Text)
getInfoForCountry c
    | length c == 1 = do
        today <- catch
                    (runReq httpConfig $ pure =<< callToday url)
                    (\(JsonHttpException _) -> return Nothing)
        yesterday <- catch
                        (runReq httpConfig $ pure =<< callYesterday url)
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
    | length c <= 5 = do -- multiple countries
        today <- catch
                    (runReq httpConfig $ pure =<< callToday url)
                    (\(JsonHttpException _) -> return Nothing)
        yesterday <- catch
                        (runReq httpConfig $ pure =<< callYesterday url)
                        (\(JsonHttpException _) -> return Nothing)
        case today of
            Nothing     -> return Nothing
            Just (info) -> do
                let Just (yestInfo) = yesterday
                    both = zip info yestInfo
                cs <- mapM (\(t, y) -> do
                        let restCountries' = restContries /: (C.iso2 . C.countryInfo $ t)
                        r <- runReq httpConfig $ do
                            req GET
                                restCountries'
                                NoReqBody
                                jsonResponse
                                mempty
                        let b = responseBody r
                        return $ (craftResponse t y b)
                        ) $ both
                return $ Just (foldl1 (\agg x -> agg <> "\n" <> x) $ cs)
    | length c > 5 = do -- bulk
        today <- catch
                    (runReq httpConfig $ pure =<< callToday url)
                    (\(JsonHttpException _) -> return Nothing)
        case today of
            Nothing     -> return Nothing
            Just (info :: [C.CountryStat]) -> do
                cs <- mapM (\t -> return $ (craftBulkResponse t)) $ info
                return $ Just (foldl1 (\agg x -> agg <> "\n" <> x) $ cs)
    | otherwise = return Nothing
    where callToday u = do
            r' <- req GET
                u
                NoReqBody
                jsonResponse
                mempty
            return (Just (responseBody r'))
          callYesterday u = do
            r' <- req GET
                u
                NoReqBody
                jsonResponse
                ("yesterday" =: ("true" :: Text))
            return (Just (responseBody r'))
          url = https "corona.lmao.ninja" /: "v2" /:"countries" /: T.intercalate "," c
          restContries = https "restcountries.eu" /: "rest" /: "v2" /: "alpha"

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
                                          percentageRecovered = (recovered' `doDiv` totalInfections) * 100
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

craftBulkResponse :: C.CountryStat -> Text
craftBulkResponse c = let countryCode = T.unpack $ C.iso2 . C.countryInfo $ c
                          deaths = (commas. show .C.deaths) c
                          critical = (commas. show . C.cases ) c
                          todayInfections = (commas. show . C.todayCases ) c
                          totalInfections = (commas. show . C.cases ) c
                          activeInfections = (commas. show . C.active ) c
                          recovered = (commas. show . C.recovered ) c
    in T.pack $ printf "%s - %s %s %s %s %s %s %s %s %s %s %s %s" 
        countryCode
        (":skull_crossbones:" :: String) deaths 
        (":biohazard:" :: String) critical 
        (":calendar:" :: String) todayInfections 
        (":nauseated_face:" :: String) totalInfections
        (":face_vomiting:" :: String) activeInfections
        (":muscle:" :: String) recovered
