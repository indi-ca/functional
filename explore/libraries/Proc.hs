
module Proc where

import Data.List(elemIndex)

import System.IO(FilePath)
import System.Directory(getDirectoryContents)
import System.Process
import System.Exit
import System.FilePath.Posix



-- What are the options for running a process?
-- proc, defined in System.Process
-- runProcess, defined in System.Process

-- readProcess, defined in System.Process
-- readProcess "/bin/ss" ["lego", "'ls'"] []

-- callCommand, defined in System.Process
-- callCommand "/bin/ss lego 'ls'"





-- EXPONENTIAL BACKOFF
-- How do I create something that tries n number of times
-- with an exponential backoff

-- I'll have to produce a bunch of y values, for a bunch of x volues
-- What is the equation for an exponential backoff? e to the power x




type SiteKey = String


data SSHRequest = SSHRequest {
    command :: String,
    result :: [String]
} deriving (Show)

data SSHRequestType = StandardSSHRequest | NBBSSHRequest


-- The configuration problem, is one that I'm constantly encountering
-- I want to taint the code, with the SiteKey, on at the last possible moment
-- However, the site key has to be passed into the inner most function
-- So, do I keep passing it in, down the call stack?


sshCommand' :: SiteKey -> String -> (String, [String])
sshCommand' sk cmd = ("/Users/indika/dev/box/internal/nb-devtools/bin/ss", [sk, cmd])

-- If it is going to block, then perhaps I can provide a timeout?
sshCommand :: SiteKey -> String -> IO (ExitCode, String, String)
sshCommand sk cmd = uncurry readProcessWithExitCode the_tuple $ []
    where the_tuple = sshCommand' sk cmd


determine_release = sshCommand "lego" "cat /etc/redhat-release"




-- Successful response is something like this:
raw_string = "connecting to lego.safenetbox.biz\nTrying 1 servers\n['10.107.11.189']\nIP address: 10.107.11.189\nGot port 4 from nbupdate SRV record\nStarting ssh connection\nNetbox release 29.6 (Final)\n"
sampleRequest = SSHRequest "cat /etc/redhat-release" (lines raw_string)

successString = "Starting ssh connection"




-- There are multiple attributes to success
-- Perhaps a fuzzy match is required
-- Perhaps with a probability
isSuccess :: SSHRequest -> Bool
isSuccess request = successString `elem` results
    where results = result request

-- get's the contents of the command
inner :: SSHRequest -> Maybe [String]
inner request = case index of (Nothing) -> Nothing
                              (Just x) -> Just (drop (x+1) results)
    where results = result request
          index = elemIndex successString results



-- First configure the request
-- Then poppualet it with the contents
-- THen parse it

doIt = sshCommand "lego" (command sampleRequest)







