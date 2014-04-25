{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log



-- parses and indivdiual line from the log file
--
-- parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"
-- parseMessage "I 29 la la la" == LogMessage Info 29 "la la la"
-- parseMessage "This is not in the right format" == Unknown "This is not in the right format"



--makeWords :: String -> [String]
--makeWords x = words x

--parseRest :: [String] -> (TimeStamp, String)
--parseRest (x:xs) = (read x :: Int, unwords xs)


-- What are all things wrong with this?
-- If there is no timestamp, or it cannot be parsed then it will fail
-- If there is no text afterwards, then it will fail
getLogMessage :: [String] -> LogMessage
getLogMessage [] = Unknown ""
getLogMessage (x:y:ys)
    | x == "I" = LogMessage Info (timestamp (y:ys)) (text (y:ys))
    | x == "W" = LogMessage Warning (timestamp (y:ys)) (text (y:ys))
    | x == "E" = LogMessage (Error (severity y)) (timestamp ys) (text ys)
    | otherwise = Unknown (unwords (x:y:ys))
    where timestamp xs = read (head xs) :: Int
          text xs = unwords (tail xs)
          severity s = read s :: Int
getLogMessage x = Unknown (unwords x)

-- How do I integrate this function with the one above
parseMessage :: String -> LogMessage
parseMessage x = getLogMessage (words x)





-- Can parse the entire file
parse :: String -> [LogMessage]
parse x = map parseMessage (lines x)

--
--testParse parse 10 "error.log"
--testParse parse 100 "error.log"
--testParse parse 5523 "error.log"
