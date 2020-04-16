{-# Language OverloadedStrings, DeriveGeneric #-}
module GlobalStats (
    GlobalStat(..)
) where
import GHC.Generics
import Data.Text
import Data.Aeson (FromJSON, ToJSON)



data GlobalStat = Global { updated :: Int
                         , cases :: Int
                         , todayCases :: Int
                         , deaths :: Int
                         , todayDeaths :: Int
                         , recovered :: Int
                         , active :: Int
                         , critical :: Int
                         , tests :: Int } deriving (Show, Generic)

instance FromJSON GlobalStat
instance ToJSON GlobalStat
