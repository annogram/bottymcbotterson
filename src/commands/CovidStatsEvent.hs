{-# Language OverloadedStrings #-}
module CovidStatsEvent 
    ( getCovidInfo
    , covidStatsCommand
    ) where
import Discord
import Discord.Types
import Data.Text
import qualified Discord.Requests as R

covidStatsCommand :: Text
covidStatsCommand = "/covid"

-- Get information on the Covid-19 pandemic
getCovidInfo :: DiscordHandle -> Event -> IO ()
getCovidInfo handle (MessageCreate m) = do 
    _ <- restCall handle $  R.CreateMessage (messageChannel m) $ "Pong!"
    pure ()