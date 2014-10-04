{-# LANGUAGE OverloadedStrings #-}

module Aesyon where

import Control.Applicative ((<$>), (<*>), empty)

--import Data.Aeson(encode, ToJSON, object)
import Data.Aeson
import Data.ByteString.Lazy.Char8(pack, unpack)
import Data.Char(toLower)



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



get_results :: Maybe Action -> Maybe [Nugget] -> Maybe [Nugget]
get_results Nothing _ = Just []
get_results _ Nothing = Just []
get_results (Just action) (Just nuggets) = Just (search (a_title action) nuggets)



respond :: String -> IO String
respond req = do
    contents <- readFile "data/data.json"
    let action = decode (pack req) :: Maybe Action
        nuggets = decode (pack contents) :: Maybe [Nugget]
        filtered = get_results action nuggets
        ret = unpack $ encode filtered
    return ret



