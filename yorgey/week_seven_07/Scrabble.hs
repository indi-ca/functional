module Scrabble where

import Data.Char (toUpper, isLetter)


-- EXCERCISE 3

-- Hence, the second annotation you decide to implement is one to
-- cache the ScrabbleTM score for every line in a buffer.
-- Create a Scrabble module that defines a Score type, a Monoid instance for Score,
-- and the following functions:

-- score :: Char -> Score
-- scoreString :: String -> Score

-- The score function should implement the tile scoring values as shown at
-- http://www.thepixiepit.co.uk/scrabble/rules.html
-- any characters not mentioned (punctuation, spaces, etc.) should be given zero points.
-- To test that you have everything working, add the line import Scrabble to the import section of your JoinList module,
-- and write the following function to test out JoinLists annotated with scores:
-- scoreLine :: String -> JoinList Score String

-- Example:
-- *JoinList> scoreLine "yay " +++ scoreLine "haskell!"
-- Append (Score 23)
--        (Single (Score 9) "yay ")
--        (Single (Score 14) "haskell!")


--score :: Char -> Score

score x
    | not $ isLetter x = 0
    | otherwise = points !! y
    where
        y = digitToInt x - 10
        letters = ['A'..'Z']
        points = [1, 3, 3, 2, 1, 4, 2, 4, 1, 8, 5, 1, 3, 1, 1, 3, 10, 1, 1, 1, 1, 4, 8, 4, 10]

