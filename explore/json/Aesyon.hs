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

data Action = Action {
    action  :: String,
    hashi   :: String,
    text    :: String
} deriving (Show)


-- Cannot do this because Nugget has kind *, not * -> *
--instance Functor Nugget where
--    fmap f (Nugget i h c l) = Nugget i h (f c) l




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
                            v .: "action" <*>
                            v .: "hash" <*>
                            v .: "text"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = empty




-- TODO: Rather return Nothing, than Just []
search :: String -> Maybe [Nugget] -> Maybe [Nugget]
search _ Nothing = Nothing
search str (Just nuggets) = Just (filter (\x -> compare (content x) str) nuggets)
    where
        compare x y = toLower (x!!0) == toLower (y!!0)

--getEncodedNuggets :: String
--getEncodedNuggets = unpack $ encode nuggets

reMapNuggets :: String -> IO String
reMapNuggets str = do
    contents <- readFile "data/data.json"
    let req = decode (pack contents) :: Maybe [Nugget]
    let filtered = search str req
    let ret = unpack $ encode filtered
    return ret





