
module Files where


import System.IO(FilePath)
import System.Directory(getDirectoryContents)


data Pattern = BasicPattern String


initial_path = "/Users/indika"
basic_pattern = BasicPattern "mslync-29.5"


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
