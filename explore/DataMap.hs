import Data.Maybe
import Data.Map as M



-- Data.Map is about mapping keys to values

-- create some keys and values

al = [(1, "one"), (2, "two"), (3, "three"), (4, "four")]
bl = [("one", 1), ("two", 2), ("three", 3), ("four", 4)]


-- There is a type defined somewhere which looks like
-- data Map k v


{- | Create a map representation of 'al' by converting the association-  list using Map.fromList -}

-- M.fromList :: Ord k => [(k, a)] -> Map k a

mapFromAL :: M.Map Integer [Char]
mapFromAL = M.fromList al

mapFromBL :: M.Map String Integer
mapFromBL = M.fromList bl

-- this is what I want to get to
-- instance HasVars (M.Map String Integer -> Maybe Integer) where

-- so the Type I'm creating an instance for is..

something :: M.Map String Integer -> Maybe Integer
something bob = Just 3
