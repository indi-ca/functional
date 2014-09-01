module Scrabble where

import Data.Char (toUpper, isLetter)
import Data.Maybe (fromJust)
import Data.Monoid

-- EXCERCISE 3

-- Hence, the second annotation you decide to implement is one to
-- cache the ScrabbleTM score for every line in a buffer.
-- Create a Scrabble module that defines a Score type, a Monoid instance for Score,

-- a Score type
newtype Score = Score Int
    deriving (Eq, Ord, Show)

getScore :: Score -> Int
getScore (Score i) = i

class Scored a where
  score :: a -> Score

instance Scored Score where
  score = id

instance Scored b => Scored (a,b) where
  score = score . snd


instance Monoid Score where
    mempty = Score 0
    mappend (Score x) (Score y) = Score (x + y)


-- and the following functions:
-- score :: Char -> Score
-- scoreString :: String -> Score

-- The score function should implement the tile scoring values as shown at
-- http://www.thepixiepit.co.uk/scrabble/rules.html
-- any characters not mentioned (punctuation, spaces, etc.) should be given zero points.


instance Scored Char where
    score x
        | not $ isLetter x = Score 0
        | otherwise = Score (fromJust $ lookup (toUpper x) (zip letters points))
        where
            letters = ['A'..'Z']
            points = [1, 3, 3, 2, 1, 4, 2, 4,
                      1, 8, 5, 1, 3, 1, 1, 3,
                      10, 1, 1, 1, 1, 4, 4, 8,
                      4, 10]

scoreString :: String -> Score
scoreString = mconcat . map score







