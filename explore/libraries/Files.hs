
module Files where


import System.IO(FilePath)
import System.Directory(getDirectoryContents)
import System.Process
import System.Exit
import System.FilePath.Posix

data Pattern = BasicPattern String


initial_path = "/Users/indika/Movies"
basic_pattern = BasicPattern "Fury"


main :: IO ()
main = do
    all_files <- getDirectoryContents initial_path
    mapM_ putStrLn (match basic_pattern all_files)



-- match a set of files based on a pattern
match :: Pattern -> [FilePath] -> [FilePath]
match (BasicPattern xs) fs = filter (startsWith xs) fs


startsWith :: Eq a => [a] -> [a] -> Bool
startsWith [] _ = True
startsWith _ [] = False
startsWith a b = and $ zipWith (==) a b



-- Finds files and returns the absolute path
find :: FilePath -> Pattern -> IO [FilePath]
find initial_path pattern = fmap (map (\x -> joinPath [initial_path, x])) matches
    where matches = fmap (match pattern) (getDirectoryContents initial_path)



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



sshCommand' :: SiteKey -> String -> (String, [String])
sshCommand' sk cmd = ("/Users/indika/dev/box/internal/nb-devtools/bin/ss", [sk, cmd])

-- If it is going to block, then perhaps I can provide a timeout?
sshCommand :: SiteKey -> String -> IO (ExitCode, String, String)
sshCommand sk cmd = uncurry readProcessWithExitCode the_tuple $ []
    where the_tuple = sshCommand' sk cmd


