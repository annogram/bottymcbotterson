module Botty.Event ( BottyEvent (..)
                   , BottyFollowUp (..)
                   , Persistent
                   ) where
import Data.Text
import Control.Concurrent
import Control.Concurrent.STM
import Data.Map.Lazy
import Discord
import Discord.Types


-- | Type alias for global persistent state
type Persistent = TVar (Map Int String)

-- | Minimal required information for a Botty event
data BottyEvent = Botty { desc :: Text -> Text
                        , cmd  :: Text
                        , func :: Text -> Persistent -> IO (Maybe Text)
                        }

data BottyFollowUp = Follow { fcmd :: Text
                            , ffunc :: DiscordHandle -> Message -> Text -> Persistent -> IO (Maybe Text)
                            }