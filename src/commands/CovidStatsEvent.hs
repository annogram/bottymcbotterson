{-# Language OverloadedStrings #-}
module CovidStatsEvent 
    ( getCovidInfo
    , covidStatsCommand
    ) where
import Discord
import Discord.Types
import qualified Discord.Requests as R
import Data.Text

covidStatsCommand :: Text
covidStatsCommand = "/covid"
-- Get information on the Covid-19 pandemic
getCovidInfo :: DiscordHandle -> Event -> IO ()
getCovidInfo handle (MessageCreate m) = do 
    _ <- restCall handle $  R.CreateMessage (messageChannel m) $ "Pong!"
    pure ()