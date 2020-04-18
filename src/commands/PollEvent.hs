{-# LANGUAGE OverloadedStrings #-}
module PollEvent 
    ( pollCommand
    , pollDesc
    , poll
    ) where
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Data.Text

-- Exposed comamnd key
pollCommand :: Text
pollCommand = "/poll"

-- Description
pollDesc :: Text
pollDesc = "/poll - Responds with pong \n"
            <> "\tUsage: /ping"

-- Functionality to reply to ping
poll :: Text -> IO (Maybe Text)
poll _ = return $ Just "Pong!"