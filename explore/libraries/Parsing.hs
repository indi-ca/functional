{-# LANGUAGE OverloadedStrings #-}

module Parsing where

import Data.Attoparsec
import Data.Attoparsec.Char8
import Data.ByteString.Internal



sample = "mslync-29.5-42.i686.rpm"


data Release = Release {
    major :: Int,
    minor :: Int
} deriving Show

data RPM = RPM {
    name :: ByteString,
    release :: Release,
    index :: Int,
    architecture :: String
} deriving Show



parseRelease :: Parser Release
parseRelease = do
    d1 <- decimal
    char '.'
    d2 <- decimal
    return $ Release d1 d2


parseRPM :: Parser RPM
parseRPM = do
    d1 <- string "mslync"
    char '-'
    d2 <- parseRelease
    char '-'
    d3 <- decimal
    return $ RPM d1 d2 d3 "oe"



main = do
    print $ parseOnly parseRPM sample
