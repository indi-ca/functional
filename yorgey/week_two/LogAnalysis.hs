{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log



-- parses and indivdiual line from the log file
--
-- parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"
-- parseMessage "I 29 la la la" == LogMessage Info 29 "la la la"
-- parseMessage "This is not in the right format" == Unknown "This is not in the right format"

--parseMessage :: String -> LogMessage
--parseMessage k sev time text = LogMessage Info time text



-- Can parse the entire file
parse :: String -> [LogMessage]
parse _ = [LogMessage Info 29 "la la la"]

--
--testParse parse 10 "error.log"


makeWords :: String -> [String]
makeWords x = words x

parseRest :: [String] -> (TimeStamp, String)
parseRest (x:xs) = (read x :: Int, unwords xs)

getMessageType :: [String] -> MessageType
getMessageType (x:y:_)
    | x == "I" = Info
    | x == "W" = Warning
    | x == "E" = Error (read y :: Int)


j = "45"
js = ["hello", "bob"]



getLogMessage :: [String] -> LogMessage
getLogMessage (x:y:ys)
    | x == "I" = LogMessage Info (fst (parseRest (y : ys))) (snd (parseRest (y : ys)))
    | x == "W" = LogMessage Warning (fst (parseRest (y : ys))) (snd (parseRest (y : ys)))
    | x == "E" = LogMessage (Error (read y :: Int)) (fst (parseRest ys)) (snd (parseRest ys))
    | otherwise = Unknown (unwords (x:y:ys))

parseMessage :: String -> LogMessage
parseMessage x = getLogMessage (words x)


input = makeWords "E 6 Completed armadillo processing"
doMatch = getMessageType input


a_message = LogMessage Info 3 "This is the message"
b_message = Unknown "This is Unknown"


baz :: LogMessage -> String
baz p@(LogMessage n _ _) = "The MessageType is " ++ show p ++ " -----> " ++ show n
