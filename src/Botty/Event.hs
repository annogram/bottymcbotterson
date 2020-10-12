module Botty.Event ( BottyEvent (..)
                   , BottyFollowUp (..)
                   , Persistent
                   ) where
import Data.Text
import Control.Concurrent
import Control.Concurrent.STM
import Data.Map.Lazy
import Data.ByteString
import Discord
import Discord.Types

data Botty = BottyMessage  | BottyReact | BottyUnreact

class BottyEvents a where
    description :: a -> Text
    function :: a -> Persistent -> IO (Maybe Text)
    command :: a -> Text


-- | Type alias for global persistent state
type Persistent = TVar (Map Int ByteString)

-- | Minimal required information for a Botty event
data BottyEvent = Botty { desc :: Text -> Text
                        , cmd  :: Text
                        , func :: Text -> Persistent -> IO (Maybe Text)
                        }

data BottyFollowUp = Follow { fcmd :: Text
                            , ffunc :: DiscordHandle -> Message -> Text -> Persistent -> IO (Maybe Text)
                            }