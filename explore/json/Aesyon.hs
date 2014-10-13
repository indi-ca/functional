{-# LANGUAGE OverloadedStrings #-}

module Aesyon where

import Control.Applicative ((<$>), (<*>), empty)

--import Data.Aeson(encode, ToJSON, object)
import Data.Aeson
import Data.Aeson.Encode.Pretty(encodePretty)
import Data.ByteString.Lazy.Char8(pack, unpack)
import Data.Char(toLower)

import Text.EditDistance(levenshteinDistance, defaultEditCosts)

import System.IO(hPutStrLn, openFile, IOMode( WriteMode ))

import System.Log.Logger
import System.Log.Handler.Syslog
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter

-- Hidden packages!
-- import System.Random


data Nugget = Nugget {
    index        :: Int,
    hash         :: String,
    kind         :: String,
    title        :: String,
    content      :: String,
    lastModified :: Int
} deriving (Show)


-- [?] How do I have two value constructors with the same name?
data Action = Action {
    a_action  :: String,
    a_kind  :: String,
    a_hash  :: String,
    a_title  :: String
} deriving (Show)




instance FromJSON Nugget where
    parseJSON (Object v) =  Nugget <$>
                            v .: "index" <*>
                            v .: "hash" <*>
                            v .: "kind" <*>
                            v .: "title" <*>
                            v .: "content" <*>
                            v .: "lastModified"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = empty

instance ToJSON Nugget where
    toJSON (Nugget index hash kind title content lastModified) = object ["index" .= index, "hash" .= hash, "kind" .= kind, "title" .= title, "content" .= content, "lastModified" .= lastModified]


instance FromJSON Action where
    parseJSON (Object v) =  Action <$>
                            v .: "a_action" <*>
                            v .: "a_kind" <*>
                            v .: "a_hash" <*>
                            v .: "a_title"
    parseJSON _          = empty



-- TODO: Rather return Nothing, than Just []
-- Just [] implies that there was no failure though
search :: String -> [Nugget] -> [Nugget]
search str nuggets = filter (\x -> compare (content x) str) nuggets
    where
        compare x y = toLower (x!!0) == toLower (y!!0)


create :: String -> Nugget
create keyword = Nugget 10 "10" "text" keyword keyword 1

-- TODO: I should be able to create on an empty list
decide_what :: Maybe Action -> Maybe [Nugget] -> Maybe [Nugget]
decide_what Nothing _ = Just []
decide_what _ Nothing = Just []
decide_what (Just action) (Just nuggets)
    | action_type == "create" = Just ((create $ a_title action) : nuggets)
    | action_type == "search" = Just (search (a_title action) nuggets)
    | action_type == "execute" = Just (search (a_title action) nuggets)
    where
        action_type = a_action action



respond :: String -> IO String
respond req = do
    contents <- readFile "data/data.json"
    let action = decode (pack req) :: Maybe Action
        nuggets = decode (pack contents) :: Maybe [Nugget]
        filtered = decide_what action nuggets
        ret = unpack $ encode filtered
        pretty_ret = unpack $ encodePretty filtered
    length contents `seq` (writeFile "data/data.json" $ pretty_ret)
    return ret



