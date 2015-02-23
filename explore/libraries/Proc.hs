
module Proc where

import Data.List(elemIndex)
import Control.Applicative ((<*>), (*>), (<*), (<$>), (<$))

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



-- Watch this: ls -l /etc/runonce


type SiteKey = String


data SSHRequest = SSHRequest {
    command :: String,
    result :: [String]
} deriving (Show)

data SSHRequestAlt = SSHRequestAlt {
    commandAlt :: String
} deriving (Show)

data SSHResponse = SSHResponse {
    resultAlt :: [String]
} deriving (Show)


data SSHRequestType = StandardSSHRequest | NBBSSHRequest


-- The configuration problem, is one that I'm constantly encountering
-- I want to taint the code, with the SiteKey, on at the last possible moment
-- However, the site key has to be passed into the inner most function
-- So, do I keep passing it in, down the call stack?


sshCommand' :: SiteKey -> String -> (String, [String])
sshCommand' sk cmd = ("ssh", ["-p 4", "-o ConnectTimeout=3", "-o BatchMode=yes", sk, cmd])
--sshCommand' sk cmd = ("/Users/indika/dev/box/internal/nb-devtools/bin/ss", ["-o ConnectTimeout=3 -o BatchMode=yes", sk, cmd])

-- This doesn't really work
-- However, it does give me an actual error
-- sshCommand' sk cmd = ("/Users/indika/dev/box/internal/nb-devtools/bin/ss", ["-o", "'BatchMode yes'", sk, cmd])

-- The first argument has to be the full command name
--sshCommand' sk cmd = ("/Users/indika/dev/box/internal/nb-devtools/bin/ss -o 'BatchMode yes'", [sk, cmd])

-- If it is going to block, then perhaps I can provide a timeout?
sshCommand :: SiteKey -> String -> IO (ExitCode, String, String)
sshCommand sk cmd = uncurry readProcessWithExitCode the_tuple $ []
    where the_tuple = sshCommand' sk cmd











-- There are multiple attributes to success
-- Perhaps a fuzzy match is required
-- Perhaps with a probability
successString = "Starting ssh connection"

isSuccess :: SSHRequest -> Bool
isSuccess request = successString `elem` results
    where results = result request

-- get's the contents of the command
inner :: SSHResponse -> Maybe [String]
inner request = case index of (Nothing) -> Nothing
                              (Just x) -> Just (drop (x+1) results)
    where results = resultAlt request
          index = elemIndex successString results

inner' :: Maybe [String] -> Maybe [String]
inner' (Nothing) = Nothing
inner' (Just xs) = case index of (Nothing) -> Nothing
                                 (Just x) -> Just (drop (x+1) xs)
    where index = elemIndex successString xs

-- First configure the request
-- Then poppualet it with the contents
-- THen parse it

-- Successful response is something like this:
raw_string = "connecting to lego.safenetbox.biz\nTrying 1 servers\n['10.107.11.189']\nIP address: 10.107.11.189\nGot port 4 from nbupdate SRV record\nStarting ssh connection\nNetbox release 29.6 (Final)\n"
sampleRequest = SSHRequest "cat /etc/redhat-release" (lines raw_string)



determine_release = sshCommand "lego" "cat /etc/redhat-release"
test = sshCommand "lego" (command sampleRequest)



-- What will the interface look like, and then work from the limitation

-- Creating it with an empty string is kind of ugly
releaseRequest = SSHRequestAlt "cat /etc/*release*"
lego = "root@lego.safenetbox.biz"

subFunction :: (ExitCode, String, String) -> IO (Either String SSHResponse)
subFunction (ExitSuccess, str_a, str_b) = return (Right (SSHResponse (lines str_a)))
subFunction ((ExitFailure x), str_a, str_b) = return (Left ("Failed with code: " ++ show x ++ "\n" ++ str_a ++ str_b))

subFunctionTwo :: (ExitCode, String, String) -> IO (Maybe [String])
subFunctionTwo (ExitSuccess, str_a, str_b) = return (Just (lines str_a))
subFunctionTwo ((ExitFailure _), _, _) = return Nothing

requestTranslator :: IO (ExitCode, String, String) -> IO (Either String SSHResponse)
requestTranslator response = response >>= subFunction

-- What's going to happen here?
performRequest :: SSHRequestAlt -> SiteKey -> IO (Either String SSHResponse)
performRequest request sitekey = requestTranslator subRequest
    where subRequest = sshCommand sitekey (commandAlt request)



-- I need two things from here
-- 1. Get the inner
-- 2. Have an exponential backoff function (that has a limit)


-- 1. How do I start with the request, and get the inner?
--something :: IO (Either String SSHResponse) -> IO (Either String (Maybe [String]))
--something

-- There are so many boiler plates
-- TODO: Change the Maybe into an Either

-- This one is direct, without the ss wrapper,
-- So, we don't have to clear out those lines

again :: SSHRequestAlt -> SiteKey -> IO (Maybe [String])
again request sitekey = subRequest >>= subFunctionTwo
    where subRequest = sshCommand sitekey (commandAlt request)

--again :: SSHRequestAlt -> SiteKey -> IO (Maybe [String])
--again request sitekey = inner' <$> (subRequest >>= subFunctionTwo)
--    where subRequest = sshCommand sitekey (commandAlt request)












