{-# LANGUAGE BangPatterns #-}


module Proc where

import Data.Time.Clock (diffUTCTime, getCurrentTime, NominalDiffTime)

import System.Exit (ExitCode)
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)


data Site = Site {
    siteKey :: String,
    url :: String,
    port :: Int,
    username :: String
} deriving Show


data RawSSHCommand = RawSSHCommand {
    command :: String
} deriving Show


-- | time the given IO action (clock time) and return a tuple
--   of the execution time and the result
timeIO :: IO a -> IO (NominalDiffTime, a)
timeIO ioa = do
    t1 <- getCurrentTime
    !a <- ioa
    t2 <- getCurrentTime
    return (diffUTCTime t2 t1, a)


oTimeout = 5
tmout = oTimeout * 1000000



-- TODO: It would be nice to know if it timed out
-- TODO: It would be nice to log the command that was executed
runTask :: Int -> [String] -> IO ( (NominalDiffTime, Maybe (ExitCode, String, String)) )
runTask tmout args = do
    (time, res) <- timeIO . timeout tmout $ readProcessWithExitCode "ssh" args []
    return (time, res)


-- SSH Specific stuff

--TODO: I could configure these two timeouts together
--NOTE: Do not escape the SSH args
makeArgs :: Site -> RawSSHCommand -> [String]
makeArgs site cmd = ["-p " ++ show (port site), "-o BatchMode=yes", target, "" ++ command cmd ++ ""]
    where target = username site ++ "@" ++ url site

-- Alternatives
-- Leaving this out for now
-- "-o ConnectTimeout=5"

--ssh user@server "$( cat <<'EOT'
--echo "These commands will be run on: $( uname -a )"
--EOT
--)"


cmd = "cat /etc/*release*"
rawString = concat ["cat << 'EOT'\necho\"", cmd, "\"\nEOT\n)"]


-- Perhaps this should return something simple first
-- TODO: Configure the tmout
performTask :: Site -> RawSSHCommand -> IO ( (NominalDiffTime, Maybe (ExitCode, String, String)) )
performTask site cmd = runTask tmout (makeArgs site cmd)



-- Objective: 1. Get the release of lego and motor
-- Objective: 2. Patch in either ss or ssh
-- Objective: 3. Change from Maybe to Either
-- Objective: 4. Have an exponential backoff function (that has a limit)




-- TEST DATA
lego = Site "lego" "lego.safenetbox.biz" 4 "root"
motor = Site "motor" "motor.safenetbox.biz" 4 "root"

releaseCommand = RawSSHCommand "cat /etc/*release*"


test = performTask lego releaseCommand
testM = performTask motor releaseCommand

