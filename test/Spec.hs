{-# LANGUAGE OverloadedStrings #-}
import Test.HUnit
import Lib
import Botty.Utils
import Data.List

main :: IO ()
main = runTestTT testList >> pure ()

moqTest = TestCase $ assertEqual "Should return 2" 2 2

divTest = TestCase $ assertEqual "should return 0.5" (1 `doDiv` 2) 0.5
divTest2 = TestCase $ assertEqual "should return 1" (1 `doDiv` 1) 1

emojiT = TestCase $ assertBool "list should contain neutral_face" ("neutral_face" `elem` emojiRange)

testList = TestList [ TestLabel "Should return 2" moqTest
                    , divTest
                    , divTest2
                    , emojiT
                    ]