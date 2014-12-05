{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PlayMonad where

import Control.Applicative
import Control.Monad


--import Data.Maybe hiding ((>>=), return)
data Maybe' a = Nothing' | Just' a
    deriving Show

instance Functor Maybe' where
    fmap _ Nothing' = Nothing'
    fmap g (Just' x) = Just' (g x)


instance Applicative Maybe' where
    pure = Just'

    Nothing' <*> _ = Nothing'
    _ <*> Nothing' = Nothing'
    Just' (g) <*> Just' (x) = Just' (g x)


instance Monad Maybe' where
    return = Just'

    -- >>= :: m a -> (a -> m b) -> m b
    (Just' x) >>= k = k x



check :: Int -> Maybe' Int
check n
    | n > 10    = Just' n
    | otherwise = Nothing'


--halve :: Int -> Maybe' Int
--halve x = x / 2


b = return 3 :: Maybe' Int
c = return 11 :: Maybe' Int



-- How do I override the default implementation of the instance
newtype SpawnList a = SpawnList [a]
    deriving (Eq, Show, Functor)

--instance Monad [] where
--    return x = [x]

