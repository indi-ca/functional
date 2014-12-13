module Files where

import Control.Applicative
import Data.List(sort, sortBy)

import System.IO(FilePath)
import System.Directory(getDirectoryContents)
import System.FilePath.Posix(joinPath)

data Pattern = BasicPattern String


initialPath = "/Users/indika/Movies"
basicPattern = BasicPattern "The"


main :: IO ()
main = do
    all_files <- getDirectoryContents initialPath
    mapM_ putStrLn (match basicPattern all_files)



-- match a set of files based on a pattern
match :: Pattern -> [FilePath] -> [FilePath]
match (BasicPattern xs) = filter (startsWith xs)


startsWith :: Eq a => [a] -> [a] -> Bool
startsWith [] _ = True
startsWith _ [] = False
startsWith a b = and $ zipWith (==) a b


-- Finds files and returns the absolute path
find :: FilePath -> Pattern -> IO [FilePath]
find initialPath pattern = fmap (map (\x -> joinPath [initialPath, x])) matches
    where matches = fmap (match pattern) (getDirectoryContents initialPath)

findSorted :: FilePath -> Pattern -> IO [FilePath]
findSorted initialPath pattern = sortDescending <$> find initialPath pattern

sortAscending :: [FilePath] -> [FilePath]
sortAscending = sort

sortDescending :: [FilePath] -> [FilePath]
sortDescending = sortBy (flip compare)
