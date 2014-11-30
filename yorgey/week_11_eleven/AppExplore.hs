{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AppExplore where

import Control.Applicative(Applicative, pure, (<*>), liftA2)


pair :: Applicative f => f a -> f b -> f (a,b)
pair = liftA2 (,)


-- There is an instance definition of Applicative for Maybe, without me having to write one

x = Just 4
y = Just 3

z = pair x y

-- How do I override the default implementation of the instance
newtype SpawnList a = SpawnList [a]
    deriving (Eq, Show, Functor)

-- [?] How can write an instance for the [] if one is already defined?
-- [?] Can I override instances?

--instance Applicative SpawnList where
--    pure x = SpawnList [x] -- deterministic value
--    (f:fs) <*> ar = fmap f ar ++ fs <*> ar

instance Applicative [] where
    pure x = [x] -- deterministic value
    (f:fs) <*> ar = fmap f ar ++ fs <*> ar

newtype ZipList a = ZipList { getZipList :: [a] }
    deriving (Eq, Show, Functor)

b = AppExplore.ZipList [4, 5]
c = AppExplore.ZipList [4, 5]
