module Botty.Event ( BottyEvent (..)
                   , BottyFollowUp (..)
                   , Persistent
                   ) where
import Data.Text
import Control.Concurrent
import Control.Concurrent.STM
import Data.Map.Lazy

-- | Type alias for global persistent state
type Persistent = TVar (Map Int String)

-- | Minimal required information for a Botty event
data BottyEvent = Botty { desc :: Text -> Text
                        , cmd  :: Text
                        , func :: Text -> Persistent -> IO (Maybe Text)
                        }

data BottyFollowUp = Follow { fcmd :: Text
                            , ffunc :: Text -> Persistent -> IO (Maybe Text)
                            }