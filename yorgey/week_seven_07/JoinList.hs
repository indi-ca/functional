import Data.List
import Data.Monoid

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
jl_y = Single 5 'e'
jl_h = Single 7 'h'
jl_ea = Append 6 jl_e jl_a
jl_yea = Append 30 jl_y jl_ea
jl_yeah = Append 210 jl_yea jl_h

-- Implementing this helper function may be helpful
-- that gets the annotation at the root of a JoinList

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m


instance Monoid Integer where
    mempty = 0
    mappend = (*)


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

-- The first annotation to try out is one for fast indexing into a JoinList
-- The idea is to cache the size (number of data elements) of each subtree
-- This can then be used at each step to determine if the desired index
-- is in the left or right branch

-- We have provided the Sized module that defines the Size type
-- which is simply a newtype wrapper around an Int
-- In order to make Sizes more accessible,
-- we



