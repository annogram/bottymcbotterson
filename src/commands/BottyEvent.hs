module BottyEvent (
    BottyEvent (..)
    ) where
import Data.Text

-- | Minimal required information for a Botty event
data BottyEvent = Botty { desc :: Text -> Text
                        , cmd  :: Text
                        , func :: Text -> IO (Maybe Text)
                        }