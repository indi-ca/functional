{-# LANGUAGE OverloadedStrings #-}

module Parsing(parseRPM, Release, RPM) where

import Data.Attoparsec.Text as A
import Data.Text


sample = "mslync-29.5-42.i686.rpm"


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
    d1 <- A.string "mslync"
    char '-'
    d2 <- parseRelease
    char '-'
    d3 <- decimal
    char '.'
    d4 <- A.string "i686"
    A.string ".rpm"
    return $ RPM d1 d2 d3 d4



main = do
    print $ A.parseOnly parseRPM sample
