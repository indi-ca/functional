module Files where

import Control.Applicative

import Data.List(sort, sortBy)
import Data.Time.Clock(UTCTime)
import Data.Ord(comparing)

import System.IO(FilePath)
import System.Directory(getDirectoryContents, getModificationTime)
import System.FilePath.Posix(joinPath)

import Parsing


data Pattern a = BasicPattern String | Parser a
--data Pattern = BasicPattern String


-- Obtains a list of files matching a pattern
-- from a directory
-- similar to what glob does

-- usage
-- provide an initial path
-- provide a pattern
-- and it will return a list of files
-- and for some reason it was important to sort these files by date

-- my only real objection to this
-- is that it does not work over ssh



initialPath = "/Users/indika/temp"
basicPattern = BasicPattern "cas"


main :: IO ()
main = do
    all_files <- getDirectoryContents initialPath
    mapM_ putStrLn (match basicPattern all_files)



-- match a set of files based on a pattern
match :: Pattern a -> [FilePath] -> [FilePath]
match (BasicPattern xs) = filter (startsWith xs)
--match (Parser a) = filter (parseOnly a)


startsWith :: Eq a => [a] -> [a] -> Bool
startsWith [] _ = True
startsWith _ [] = False
startsWith a b = and $ zipWith (==) a b


-- Finds files and returns the absolute path
find :: FilePath -> Pattern a -> IO [FilePath]
find initialPath pattern = fmap (map (\x -> joinPath [initialPath, x])) matches
    where matches = fmap (match pattern) (getDirectoryContents initialPath)


-- Provides sorting on file names
findAndSortByName :: FilePath -> Pattern a -> IO [FilePath]
findAndSortByName initialPath pattern = sortAscending <$> find initialPath pattern

sortAscending :: [FilePath] -> [FilePath]
sortAscending = sort

sortDescending :: [FilePath] -> [FilePath]
sortDescending = sortBy (flip compare)


-- Provides sorting on file by modification date
getModificationTime' :: FilePath ->  IO(FilePath, UTCTime)
getModificationTime' fp = fmap (\x -> (fp, x)) (getModificationTime fp)

findAndSortByDate :: FilePath -> Pattern a -> IO [(FilePath, UTCTime)]
findAndSortByDate fp pattern = fmap (sortBy (comparing snd)) files
    where
        files = find fp pattern >>= (sequence . fmap getModificationTime')






