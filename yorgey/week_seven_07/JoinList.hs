{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


import Data.List
import Data.Monoid
import Sized

-- What's going on?
-- I have an bunch of text
-- and I want to make a calculation of the text
-- I want to be able move to any spot in the corpus of text
-- and not recalculate linearly on every move
-- so, then we define a new concept
-- something that holds data, and caches meta data
-- caching, tells me of something mutable

-- this data structure has advantages

-- a light weight tree like structure
-- for holding the data
-- and caching the metadata
--
-- it's either empty,
-- or it is a node
-- or it is a left and a right subtree
-- note, that there is no node in the middle
data JoinListBasic a = BasicEmpty
                    | BasicSingle a
                    | BasicAppend (JoinListBasic a) (JoinListBasic a)


-- the intent of this data structure
-- is to directly represent append operations
-- as data constructors

-- this has the advantage of making append an 0 (1) operation
-- sticking two JoinLists together simply involves
-- applying the Append data constructor

-- making this notion more explicit
jlbToList :: JoinListBasic a -> [a]
jlbToList BasicEmpty          = []
jlbToList (BasicSingle a)     = [a]
jlbToList (BasicAppend l1 l2) = jlbToList l1 ++ jlbToList l2


-- if jl is a JoinList
-- we can think of it as a representation of the list
-- jlbToList jl, where some append operations have been deferred

-- "joining a list with products"

-- this structure makes sense for text editing appplications
-- as it provides a way of breaking the document data into pieces
-- that can be processed individually,
-- rather than having to traverse the entire document
-- this structure is what I will be annotating with the metadata
-- I want to track


-- Monoidally Annotated Join-Lists
-- For this assignment, I am going to use:


data JoinList m a = Empty
                | Single m a
                | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

-- The m parameter will be used to track monoidal annotations to the structure
-- The idea is that the annotation at the root of a JoinList
-- will aways be equal to the combinations of all the annotations on the Single nodes
-- (according to whatever notion of 'combining' is defined for the monoid in question)
-- Empty nodes do not explicity store an annotation
-- but we consider them to have an annotation of mempty
-- (that is, the identiy element for the give monoid)

--example = Append (Product 210)
--   (Append (Product 30)
--     (Single (Product 5) 'y')
--     (Append (Product 6)
--       (Single (Product 2) 'e')
--       (Single (Product 3) 'a')))
--   (Single (Product 7) 'h')

-- The above example is a JoinList
-- storing four values

-- A multiplicative monoid is being used,
-- each Append node stores the product of all the annotations below it

-- The point of doing this is that all the subcomputations needed to compute
-- the product of all the annotations in the join-list are cached

-- If we now change one of the annotations,
-- we need only recompute the annotations on nodes above it in the tree
-- the stuff below it have already been cached

-- this means, for balanced join-lists,
-- it takes only O(log n) time to rebuild the annotations after making an edit


-- EXERCISE 1

-- Consider how to write some simple operations on these JoinLists
-- The most important operation we will consider is how to append two JoinLists

-- We said that the point of JoinLists is to represent append operations as data
-- but what about annotations?



jl_e = Single 2 'e'
jl_a = Single 3 'a'
jl_y = Single 5 'y'
jl_h = Single 7 'h'
jl_ea = Append 6 jl_e jl_a
jl_yea = Append 30 jl_y jl_ea
jl_yeah = Append 210 jl_yea jl_h

fi_e = Single (1 :: Int) 'e'
fi_a = Single (1 :: Int) 'a'
fi_y = Single (1 :: Int) 'y'
fi_h = Single (1 :: Int) 'h'
fi_ea = Append (2 :: Int) fi_e fi_a
fi_yea = Append (3 :: Int) fi_y fi_ea
fi_yeah = Append (4 :: Int) fi_yea fi_h




-- Implementing this helper function may be helpful
-- that gets the annotation at the root of a JoinList

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

instance Monoid Int where
    mempty = 0
    mappend = (+)


-- Write an append function for JoinLists that yields a new JoinList
-- whose monoidal annotation is derived from those of the two arguments

--(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
--(+++) x@(Single m1 _) y@(Single m2 _) = Append (m1 `mappend` m2) x y
--(+++) x@(Single m1 _) y@(Append m2 _ _) = Append (m1 `mappend` m2) x y
--(+++) x@(Append m1 _ _) y@(Single m2 _) = Append (m1 `mappend` m2) x y
--(+++) x@(Append m1 _ _) y@(Append m2 _ _) = Append (m1 `mappend` m2) x y

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) x y = Append (tag x `mappend` tag y) x y




-- EXERCISE 2

-- Trying out an annotion. The first one is for fast indexing, into a JoinList
-- Cache the size (number of data elements) of each subtree
-- Use this cache to determine if the 'desired index' is in the left or right branch

-- We have provided the Sized module that defines the Size type
-- this is simply a newtype wrapper around an Int
-- The Sized type class has been defined
-- to make Sizes more accesible,
-- by providing a method for obtaining a Size from a value

-- Use the Sized type class to write the following functions.


-- 1. Implement the function
--   indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a

-- indexJ finds the JoinList element at the specified index.
-- If the index is out of bounds, the function returns Nothing.
-- By an index in a JoinList we mean the index in the list that it represents.
-- That is, consider a safe list indexing function


-- (!!?) :: [a] -> Int -> Maybe a
-- []       !!? _         = Nothing
-- _        !!? i | i < 0 = Nothing
-- (x:xs)   !!? 0         = Just x
-- (x:xs)   !!? i         = xs !!? (i-1)

-- which returns Just the ith element in a list (starting at zero)
-- if such an element exists, or Nothing otherwise.

-- We also consider an updated function for converting join-lists into lists,
-- just like jlbToList but ignoring the monoidal annotations:

-- jlToList :: JoinList m a -> [a]
-- jlToList Empty            = []
-- jlToList (Single _ a)     = [a]
-- jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- We can now specify the desired behavior of indexJ.
-- For any index i and join-list jl, it should be the case that
--   (indexJ i jl) == (jlToList jl !!? i)

-- That is, calling indexJ on a join-list is the same as first converting
-- the join-list to a list and then indexing into the list.
-- The point, of course, is that indexJ can be more efficient (O(log n) versus O(n),
-- assuming a balanced join-list),
-- because it gets to use the size annotations to throw away whole parts of the tree at once,
-- whereas the list indexing operation has to walk over every element.

instance Sized Int where
    size x = Size x

containsLeft :: Int -> Size -> Size -> Bool
containsLeft index (Size l) (Size r) = 1 <= index && index <= l

containsRight :: Int -> Size -> Size -> Bool
containsRight index (Size l) (Size r) = l + 1 <= index && index <= l + r

coversLeft :: Int -> Size -> Size -> Bool
coversLeft index (Size l) (Size r) = 1 + index <= 1 + l

coversRight :: Int -> Size -> Size -> Bool
coversRight index (Size l) (Size r) = 1 + index > 1 + l


containsSplit :: Int -> Size -> Size -> Bool
containsSplit index x y = (containsLeft index x y) && (containsRight index x y)

decrementIndex :: Int -> Size -> Int
decrementIndex index (Size x) = index - x


-- [!] Issue with zero indexing here

indexJ :: (Sized m, Monoid m) => Int -> JoinList m a -> Maybe a
indexJ _ Empty = Nothing
indexJ _ (Single m a) = Just a
indexJ index (Append m left right)
    | containsLeft index left_mass right_mass = indexJ index left
    | containsRight index left_mass right_mass = indexJ new_index right
    | otherwise = Nothing
    where
        left_mass = size $ tag left
        right_mass = size $ tag right
        new_index = decrementIndex index (size $ tag left)




-- 2. Implement the function
-- dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
-- The dropJ function drops the first n elements from a JoinList.
-- This is analogous to the standard drop function on lists.
-- Formally, dropJ should behave in such a way that
-- jlToList (dropJ n jl) == drop n (jlToList jl).







-- 3. Finally, implement the function
-- takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a

-- The takeJ function returns the first n elements of a JoinList, dropping all other elements.
-- Again, this function works similarly to the standard library take function; that is, it should be the case that
--    jlToList (takeJ n jl) == take n (jlToList jl).

-- Ensure that your function definitions use the size function from the Sized type class
-- to make smart decisions about how to descend into the JoinList tree.

-- [?] If I put don't cares around m and a, in the Single deconstruction, and bind a variable, does it make it lazy?
-- [?] Do guards break on first match?

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ 0 _ = Empty
takeJ _ Empty = Empty
takeJ _ x@(Single _ _) = x
takeJ index (Append m left right)
    | coversRight index left_mass right_mass = Append new_m (takeJ index left) (takeJ new_index right)
    | coversLeft index left_mass right_mass = takeJ index left
    | otherwise = Empty
    where
        left_mass = size $ tag left
        right_mass = size $ tag right
        new_index = decrementIndex index (size $ tag left)
        new_m = m










-- AN EXCURSION INTO NEW TYPES

-- Deriving an instance of Integer for mappend
-- But I cannot do this, because I've already defined mappend for Integer
-- to be the product

-- So, I'm going to create a new type


newtype AlphaInteger a = AlphaInteger a
    deriving (Eq, Ord, Num, Show)

getAlpha :: AlphaInteger a -> a
getAlpha (AlphaInteger a) = a


instance Num a => Monoid (AlphaInteger a) where
    mappend = (+)
    mempty = AlphaInteger 0


lst :: [Integer]
lst = [1,5,8,23,423,99]

prod ::Integer
prod = getAlpha . mconcat . map AlphaInteger $ lst




data Range = Range Integer Integer
    deriving Show

data Contains = ContainsNot | ContainsIn | ContainsPerfect
    deriving Show

contains :: Range -> Range -> Contains
contains (Range x1 x2) (Range y1 y2)
    | (x1 == y1) && (x2 == y2) = ContainsPerfect
    | (x1 >= y1) && (x2 < y2) = ContainsIn
    | (x1 > y1) && (x2 <= y2) = ContainsIn
    | otherwise = ContainsNot




-- [?] How do I do in depth pattern matching?








