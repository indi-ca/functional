{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PlayMonad where

import Control.Applicative
import Control.Monad


--import Data.Maybe hiding ((>>=), return)
data Maybe' a = Nothing' | Just' a


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
