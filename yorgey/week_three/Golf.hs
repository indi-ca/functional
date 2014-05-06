-- CIS 194 Homework 3

module Golf where

import Data.List

-- Skips
-- The output is a list of lists
-- The first list in the output should be the same as the input list
-- The second list in the output should contain every second element from the input list
-- and the nth list in the output should contain every nth element

-- skips "ABCD"       == ["ABCD", "BD", "C", "D"]
-- skips "hello!"     == ["hello!", "el!", "l!", "l", "o", "!"]
-- skips [1]          == [[1]]
-- skips [True,False] == [[True,False], [False]]
-- skips []           == []

-- the output should be the same length as the input


scatter :: Int -> [t] -> [t]
scatter n x = [x | (x,y) <- zipped, y == True ]
    where zipped = zip (x) (mask n (length x))

scatter' :: ([t], Int) -> [t]
scatter' (x, n) = [x | (x,y) <- zipped, y == True ]
    where zipped = zip (x) (mask n (length x))

mask :: Int -> Int -> [Bool]
mask n l = take l (cycle pattern)
    where pattern = (replicate (n-1) (False)) ++ [True]

skips x = map scatter' (source x)
    where source x = zip (replicate (length x) x) ([1..(length x)])



-- Try a different perspective
-- Create a function that creates a function for each item

--a = [1..10]
a = [1,4,5,4,6,6,3,4,2,4,9]
h = replicate 10 0

generator :: [t] -> Int ->  [t]
generator xs n = xs

some_func xs = map gen source
    where gen = generator xs
          source = [1.. (length xs)]


-- Local Maxima
-- A local maximum of a list
-- is an element of the list which is strictly
-- greater than both the elements
-- immediately before and after it.

-- For example, in the list [2,3,4,1,5], the only local maximum is 4,
-- since it is greater than the elements immediately before and after it (3 and 1).
-- 5 is not a local maximum since there is no element that comes after it.

--localMaxima [2,9,5,6,1] == [9,6]
--localMaxima [2,3,4,1,5] == [4]
--localMaxima [1,2,3,4,5] == []

f :: Ord t => t -> t -> t -> [t]
f a b c
    | b > a && b > c = [b]
    | otherwise = []

 --[x, head xs, last xs]
 --This is the local function that support localMaxima
local :: Ord a => [a] -> [a]
local [] = []
local s@(x:xs)
    | length s == 3 = f x (head xs) (last xs)
    | otherwise = []


prepr :: [a] -> [([a], Int)]
prepr xs = zip (replicate (length xs) xs) indexes
    where indexes = [0..((length xs) - 1)]

source :: [a] -> [[a]]
source xs = map subsets (prepr xs)
    where subsets (xs, n) = take 3 (drop n xs)

localMaxima :: [Integer] -> [Integer]
localMaxima xs = concat (map local (source xs))



-- which takes as input a list of Integers between 0 and 9 (inclusive),
-- and outputs a vertical histogram showing how many of each number were in the input list.
-- You may assume that the input list does not contain any numbers less than zero
-- or greater than 9
-- (that is, it does not matter what your function does if the input does contain such numbers).
-- Your output must exactly match the output shown in the examples below.

-- histogram [1,1,1,5] ==
-- *
-- *
-- *   *
-- ==========
-- 0123456789


-- histogram [1,4,5,4,6,6,3,4,2,4,9] ==
--     *
--     *
--     * *
--  ******  *
-- ==========
-- 0123456789


-- Important note: If you type something like histogram [3,5] at the ghci prompt,
-- you should see something like this:
-- "   * *    \n==========\n0123456789\n"

-- This is a textual representation of the String output,
-- including \n escape sequences to indicate newline characters.
-- To actually visualize the histogram as in the examples above, use putStr,
-- for example, putStr (histogram [3,5]).

--replaceInList :: Int -> a -> [a] -> [a]
replaceInList n newVal (x:xs)
     | n == 0 = newVal:xs
     | otherwise = x:replaceInList (n-1) newVal xs


increment :: Int -> [Int] -> [Int]
increment n xs = replaceInList n ((xs !! n) + 1) xs

-- takes a list of numbers, and a histogram, and returns a histogram
hister :: [Int] -> [Int] -> [Int]
hister [] hist = hist
hister (x:xs) hist = hister xs (increment x hist)


-- a value, and a max padding
render :: Int -> Int -> [Char]
render m x = replicate x '*' ++ replicate (m - x) ' '


-- Note, how I can reference source before I have defined it
-- Note, to avoid tuples, I can partially apply a function
drawLines = map render' source
    where render' = render (maximum source)
          source = hister a h

finalDrawLines xs = map render' xs
    where render' = render (maximum xs)



finally = concatMap (\x -> x ++ "\n") rendered ++ "==========\n" ++ "0123456789\n"
    where rendered = (reverse (transpose drawLines))


finalRender :: [Int] -> [Char]
finalRender xs = concatMap (\x -> x ++ "\n") serialized ++ "==========\n" ++ "0123456789\n"
    where serialized = reverse (transpose (finalDrawLines xs))


histogram :: [Int] -> String
histogram xs = finalRender source
    where source = hister xs h

-- might have to do a transpose
