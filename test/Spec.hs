{-# LANGUAGE OverloadedStrings #-}
import Test.HUnit
import System.Exit (exitSuccess, exitFailure)
import Lib
import Botty.Utils
import Data.List
import Botty.Event
import Botty.Commands.PongEvent
import Botty.Commands.CovidStatsEvent
import Botty.Commands.PollEvent
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Text as T
import qualified Data.Map.Lazy as M


blankPersistent :: IO Persistent
blankPersistent = newTVarIO $ M.empty

-- | Exit failure to stop job in gitlab
main :: IO Int
main = runTestTT testList >>= \c -> if errors c + failures c == 0
    then exitSuccess 
    else exitFailure

moqTest = TestCase $ assertEqual "Should return 2" 2 2

divTest = TestCase $ assertEqual "should return 0.5" (1 `doDiv` 2) 0.5
divTest2 = TestCase $ assertEqual "should return 1" (1 `doDiv` 1) 1

emojiT = TestCase $ assertBool "list should contain neutral_face" ("neutral_face" `elem` emojiRange)

pongT = TestCase $ do
    p <- blankPersistent
    assertEqual "Ping command" "/ping" (cmd pongEvent)
    Just (f) <- func pongEvent "" p
    assertEqual "Pong response" "Pong!" f

covidT = TestCase $ do
    p <- blankPersistent
    assertEqual "Covid command" "/covid" (cmd covidEvent)
    Just (f) <- func covidEvent "/covid" p
    assertBool "Covid response deaths" ("Deaths" `T.isInfixOf` f)
    assertBool "Covid response Infections" ("Infections" `T.isInfixOf` f)
    assertBool "🤮 in response" (":face_vomiting:" `T.isInfixOf` f)

covidCountryT = TestCase $ do
    p <- blankPersistent
    assertEqual "Covid command" "/covid" (cmd covidEvent)
    Just (f) <- func covidEvent "/covid nz" p
    assertBool "Covid response deaths" ("Deaths" `T.isInfixOf` f)
    assertBool "Covid response Infections" ("Infections" `T.isInfixOf` f)
    assertBool "🤮 in response" (":face_vomiting:" `T.isInfixOf` f)
    assertBool "Covid response Recovered" ("%" `T.isInfixOf` f)
    assertBool "New Zealand in title" ("New Zealand's" `T.isInfixOf` f)

multipleCovidCountryT = TestCase $ do
    p <- blankPersistent
    Just (f) <- func covidEvent "/covid nz,mexico" p
    assertBool "Two blocks" ((length . T.lines) f == 15)
    assertBool "New Zealand in title" ("New Zealand's" `T.isInfixOf` f)
    assertBool "Mexico in title" ("Mexico's" `T.isInfixOf` f)

pollT = TestCase $ do
    -- Arrange
    let options = ["yip yip", "cha cha cha", "yahe yahe yahe yo"]
        commandOpts = foldl1 (\agg x -> agg <> "," <> x) options
    p <- blankPersistent
    -- Act
    Just (f) <- func pollEvent ("/poll What does the fox say? (" <> commandOpts <> ")") p
    -- Assert
    assertBool "options in response" $ foldr (\x agg -> agg && (x `T.isInfixOf` f)) True options
    after <- readTVarIO p
    assertBool "persitence updated" $ (length . M.elems) after == 1

malformedPollT = TestCase $ do
    -- Arrange
    let options = ["yip yip", "cha cha cha", "yahe yahe yahe yo"]
        commandOpts = foldl1 (\agg x -> agg <> "," <> x) options
    p <- blankPersistent
    -- Act
    Just (f) <- func pollEvent "/poll No options" p
    -- Assert
    assertEqual "Responds with usage" "/poll - Starts a poll with options \n\tUsage: /poll" f
    after <- readTVarIO p
    assertBool "persitence updated" $ (length . M.elems) after == 0

testList = TestList [ TestLabel "Should return 2" moqTest
                    , divTest
                    , divTest2
                    , emojiT
                    , pongT
                    , covidT
                    , covidCountryT
                    , multipleCovidCountryT
                    , pollT
                    , malformedPollT
                    ]