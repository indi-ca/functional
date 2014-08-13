import Data.List


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





