
module Proc where


import System.IO(FilePath)
import System.Directory(getDirectoryContents)
import System.Process
import System.Exit
import System.FilePath.Posix



--createProcess (proc "ss lego 'ls'" [])
--createProcess (proc "./Users/indika/dev/box/internal/nb-devtools/bin/ss lego 'ls'" [])

--runProcess "/Users/indika/dev/box/internal/nb-devtools/bin/ss" []

-- but it could fail, so, it should be tried again
--callCommand "/Users/indika/dev/box/internal/nb-devtools/bin/ss lego 'ls'"

--readProcess "/Users/indika/dev/box/internal/nb-devtools/bin/ss" ["lego", "'ls'"] []


type SiteKey = String


-- This is a recurring problem I try to solve
-- I want to configure it / taint it / with a site key
-- Perhaps it should be the last parameter
-- Because i'll want to change the same infrastructure for different sites



-- How do I create something that tries n number of times
-- with an exponential backoff



-- How about I open up an SSH connection
-- and run processes through there
-- but there is only one connection
-- so this becomes a shared resource

sshCommand' :: SiteKey -> String -> (String, [String])
sshCommand' sk cmd = ("/Users/indika/dev/box/internal/nb-devtools/bin/ss", [sk, cmd])

-- If it is going to block, then perhaps I can provide a timeout?
sshCommand :: SiteKey -> String -> IO (ExitCode, String, String)
sshCommand sk cmd = uncurry readProcessWithExitCode the_tuple $ []
    where the_tuple = sshCommand' sk cmd


