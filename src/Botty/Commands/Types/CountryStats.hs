{-# Language DeriveGeneric #-}
module Botty.Commands.Types.CountryStats ( CountryStat(..)
                    , Population(..)
                    , CountryInfo(..)
                    ) where
import GHC.Generics
import Data.Text
import Data.Aeson (FromJSON, ToJSON)

data CountryInfo = Info { iso2 :: Text } deriving (Show, Generic)

instance FromJSON CountryInfo
instance ToJSON CountryInfo

data CountryStat = CountryStat { updated :: Int
                               , country :: Text
                               , countryInfo :: CountryInfo
                               , cases :: Int
                               , todayCases :: Int
                               , deaths :: Int
                               , todayDeaths :: Int
                               , recovered :: Int
                               , active :: Int
                               , critical :: Int
                               , tests :: Int
                               } deriving (Show, Generic)


instance FromJSON CountryStat
instance ToJSON CountryStat

data Population = Population {population :: Int} deriving (Show, Generic)

instance FromJSON Population
instance ToJSON Population