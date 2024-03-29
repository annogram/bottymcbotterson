module Botty.Utils ( emojiRange
                   , randomEmoji
                   , doDiv
                   , discordSyn
                   ) where

import Data.Char
import Numeric
import Data.List
import Text.Emoji
import System.Random
import Data.Function (on)
import qualified Data.Text as T

discordSyn :: T.Text -> T.Text
discordSyn x = let Just (e:_) = aliasesFromEmoji x
               in e

emojiRange :: [T.Text]
emojiRange = let ranges = (\(a,b) -> [readH a .. readH b]) <$> [ ("1F300", "1F320")
                          , ("1F330", "1F335")
                          , ("1F337", "1F37C")
                          , ("1F380", "1F393")
                          , ("1F3A0", "1F3C4")
                          , ("1F3C6", "1F3CA")
                          , ("1F3E0", "1F3F0")
                          , ("1F400", "1F43E")
                          , ("1F442", "1F4F7")
                          , ("1F5FB", "1F636")
                          ] 
             in (discordSyn . T.singleton . chr) <$> concat ranges
    where readH c = let (n,_):[] = readHex c
                    in n

randomEmoji :: IO T.Text
randomEmoji = randomRIO(0, length emojiRange) >>= \i -> return $ emojiRange !! i

doDiv :: (Integral a) => a -> a -> Double
doDiv = (/) `on` fromIntegral
