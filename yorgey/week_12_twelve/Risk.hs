{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom



-- The die function simulates the roll of a die, picking a number between 1 and 6, inclusive,
-- and returning it in the Rand monad. Notice that this code will work with any source of random numbers g.

die' :: (RandomGen g) => Rand g Int
die' = getRandomR (1,6)
--The dice function uses replicate and sequence to simulate the roll of n dice.

dice :: (RandomGen g) => Int -> Rand g [Int]
dice n = sequence (replicate n die')
--To extract a value from the Rand monad, we can can use evalRandIO.

main = do
  values <- evalRandIO (dice 2)
  putStrLn (show values)


------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }



-- EXCERCISE 2

-- write a function of this type

-- What does this do?
-- Which creates the result of a random battle
--battle :: Battlefield -> Rand StdGen Battlefield


-- Now I know, how to create the result of a die roll


die_roll :: (RandomGen g) => Rand g Int
die_roll = getRandomR (1, 6)

die_roll' :: Rand StdGen Int
die_roll' = getRandomR (1, 6)



-- Rand is, apparently a Monad,
foo :: Int -> Rand StdGen Int
foo x = return x



-- therefore, I can call return on it

--bob ::  Int
--bob = return die_roll'

-- I can also bind them together
--something = die_roll' (>>=) die_roll'







-- What are the rules of the battle?
-- There is the attacker, and there is the defender

--instance Monad Rand where
--    --return
--    ma (>>=) mb =

-- What does it mean to derive something



-- Cannot do this
--data Apple a = Apple Int a
--    deriving (Monad)












