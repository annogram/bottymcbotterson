module Botty.Shedule.TaskRegister ( registerTask ) where
import System.Cron
import Data.Text

-- | Register a task to execute at some interval in the future.
--   Standard cron notaiton
--   ┌───────────── minute (0 - 59)
--   │ ┌───────────── hour (0 - 23)
--   │ │ ┌───────────── day of the month (1 - 31)
--   │ │ │ ┌───────────── month (1 - 12)
--   │ │ │ │ ┌───────────── day of the week (0 - 6) (Sunday to Saturday;
--   │ │ │ │ │                                   7 is also Sunday on some systems)
--   │ │ │ │ │
--   │ │ │ │ │
--   * * * * * <IO Monad>
registerTask :: Text -> IO () -> IO ()
registerTask scd task = do 
    _ <- execSchedule $ addJob task scd
    return ()