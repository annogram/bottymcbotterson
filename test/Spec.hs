{-# LANGUAGE OverloadedStrings #-}
import Test.HUnit
import Lib
import Botty.Utils
import Data.List
import PongEvent
import Botty.Event
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Map.Lazy as M


blankPersistent :: IO Persistent
blankPersistent = newTVarIO $ M.fromList []

main :: IO ()
main = runTestTT testList >> pure ()

moqTest = TestCase $ assertEqual "Should return 2" 2 3

divTest = TestCase $ assertEqual "should return 0.5" (1 `doDiv` 2) 0.5
divTest2 = TestCase $ assertEqual "should return 1" (1 `doDiv` 1) 1

emojiT = TestCase $ assertBool "list should contain neutral_face" ("neutral_face" `elem` emojiRange)

pongTest = TestCase $ do
    p <- blankPersistent
    assertEqual "Ping command" "/ping" (cmd pongEvent)
    Just (f) <- func pongEvent "" p
    assertEqual "Pong response" "Pong!" f

testList = TestList [ TestLabel "Should return 2" moqTest
                    , divTest
                    , divTest2
                    , emojiT
                    , TestLabel "Pong event" pongTest
                    ]