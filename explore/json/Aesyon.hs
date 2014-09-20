{-# LANGUAGE OverloadedStrings #-}

module Aesyon where

import Control.Applicative ((<$>), (<*>), empty)

--import Data.Aeson(encode, ToJSON, object)
import Data.Aeson

import Data.ByteString.Lazy.Char8(unpack)


data Nugget = Nugget {
    index        :: Int,
    hash         :: String,
    content      :: String,
    lastModified :: Int
} deriving (Show)




instance FromJSON Nugget where
    parseJSON (Object v) =  Nugget <$>
                            v .: "index" <*>
                            v .: "hash" <*>
                            v .: "content" <*>
                            v .: "lastModified"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = empty


instance ToJSON Nugget where
    toJSON (Nugget index hash content lastModified) = object ["index" .= index, "hash" .= hash, "content" .= content, "lastModified" .= lastModified]


n1 = Nugget {
    index = 1,
    hash = "1",
    content = "Content one",
    lastModified = 1
}

n2 = Nugget {
    index = 2,
    hash = "2",
    content = "Content two",
    lastModified = 1
}

n3 = Nugget {
    index = 3,
    hash = "3",
    content = "Content three",
    lastModified = 1
}


nuggets = [n1, n2, n3]


--strictToLazy strict = BL.fromChunks [strict]
--lazyToStrict lazy = B.concat $ BL.toChunks lazy
--lazyToStrict lazy = BLC.concat $ BLC.toChunks lazy

getEncodedNuggets :: String
getEncodedNuggets = unpack $ encode nuggets
--getEncodedNuggets = BLC.unpack $ encode nuggets
--getEncodedNuggets = BLC.unpack $ lazyToStrict $ encode nuggets
--getEncodedNuggets = BC.unpack $ encode nuggets

