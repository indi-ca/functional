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
-- To test that you have everything working, add the line import Scrabble to the
-- import section of your JoinList module,
-- and write the following function to test out JoinLists annotated with scores:
-- scoreLine :: String -> JoinList Score String

-- Example:
-- *JoinList> scoreLine "yay " +++ scoreLine "haskell!"
-- Append (Score 23)
--        (Single (Score 9) "yay ")
--        (Single (Score 14) "haskell!")


--score :: Char -> Score


-- a Score type
data Score = Score Int


--score :: Char -> Score
score x
    | not $ isLetter x = 0
    | otherwise = points !! y
    where
        y = digitToInt x - 10
        letters = ['A'..'Z']
        points = [1, 3, 3, 2, 1, 4, 2, 4, 1, 8, 5, 1, 3, 1, 1, 3, 10, 1, 1, 1, 1, 4, 8, 4, 10]



-- EXCERCISE 4

-- Finally, combine these two kinds of annotations.
-- A pair of monoids is itself a monoid:
--  instance (Monoid a, Monoid b) => Monoid (a,b) where
--    mempty = (mempty, mempty)
--    mappend (a1,b1) (a2,b2) = (mappend a1 a2, mappend b1 b2)

-- (This instance is defined in Data.Monoid.)
-- This means that join-lists can track more than one type of annotation at once,
-- in parallel, simply by using a pair type.
-- Since we want to track both the size and score of a buffer,
-- you should provide a Buffer instance for the type
-- JoinList (Score, Size) String.

-- Due to the use of the Sized type class,
-- this type will continue to work with your functions such as indexJ.
-- Finally, make a main function to run the editor interface using your
-- join-list backend in place of the slow String backend
-- (see StringBufEditor.hs for an example of how to do this).
-- You should create an initial buffer of type JoinList (Score, Size) String and
-- pass it as an argument to runEditor editor.
-- Verify that the editor demonstration described in the section “Editors and Buffers”
-- does not exhibit delays when showing the prompt.
