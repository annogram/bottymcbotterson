module Botty.Utils (
    emojiRange
) where

import Data.Char
import Numeric
import Data.List
import Text.Emoji
import qualified Data.Text as T

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
                          ] 
             in (discordSyn . T.singleton . chr) <$> concat ranges
    where readH c = let (n,_):[] = readHex c
                    in n
          discordSyn x = let Just (e:_) = aliasesFromEmoji x
                         in e