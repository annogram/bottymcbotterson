module Lib
    ( someFunc
    ) where
import Discord
import Discord.Types
import qualified Discord.Requests as R

someFunc :: IO ()
someFunc = putStrLn "someFunc"
