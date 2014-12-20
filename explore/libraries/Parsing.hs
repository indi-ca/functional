--{-# LANGUAGE OverloadedStrings #-}

module Parsing (RPM, Release, parseRPM) where

import qualified Data.Attoparsec as A
import Data.Attoparsec.Text(char, decimal)
--import Data.Attoparsec.Char8
--import Data.ByteString.Internal
--import Data.ByteString
--import Data.ByteString.Char8(pack)
import Data.Text(Text, pack)

sample = pack "mslync-29.5-42.i686.rpm"


data Release = Release {
    major :: Int,
    minor :: Int
} deriving Show

data RPM = RPM {
    name :: Text,
    release :: Release,
    index :: Int,
    architecture :: Text
} deriving Show


parseRelease :: A.Parser Release
parseRelease = do
    d1 <- decimal
    char '.'
    d2 <- decimal
    return $ Release d1 d2


parseRPM :: A.Parser RPM
parseRPM = do
    d1 <- A.string (pack "mslync")
    char '-'
    d2 <- parseRelease
    char '-'
    d3 <- decimal
    char '.'
    d4 <- A.string (pack "i686")
    A.string (pack ".rpm")
    return $ RPM d1 d2 d3 d4



--parse' :: Parser a -> String -> a
--parse' parser str = parseOnly (pack str)

--main = do
--    print $ A.parseOnly parseRPM sample
