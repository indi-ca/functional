{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LyncLogAnalysis where

import Data.Attoparsec.Text
import Data.Text(Text)
import System.IO(hGetContents, IOMode(ReadMode), openFile)


newtype Date = Date String
newtype Module = Module String
newtype Level = Level String
newtype Message = Message String

data LogLine = LogLine Date Module Level Message



parseLogLine :: Parser LogLine
parseLogLine = do
    return $ LogLine (Date "date") (Module "module") (Level "level") (Message "message")


logParseLine :: Text -> Either String LogLine
logParseLine str = parseOnly parseLogLine str


something :: String -> [String]
something str = lines str


main :: IO ()
main = do
    inputHandle <- openFile "sample.log" ReadMode
    contents <- hGetContents inputHandle
    --something contents
    putStrLn "Done"
