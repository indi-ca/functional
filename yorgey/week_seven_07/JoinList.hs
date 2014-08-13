import Data.List


-- a light weight tree like structure
-- for holding the data
-- and caching the metadata
data JoinListBasic a = Empty
                    | Single a
                    | Append (JoinListBasic a) (JoinListBasic a)


-- the intent of this data structure
-- is to directly represent append operations
-- as data constructors

-- this has the advantage of making append an 0 (1) operation
-- sticking two JoinLists together simply involves
-- applying the Append data constructor

-- making this notion more explicit
