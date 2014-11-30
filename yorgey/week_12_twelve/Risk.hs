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

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }


--The die function simulates the roll of a die, picking a number between 1 and 6, inclusive, and returning it in the Rand monad. Notice that this code will work with any source of random numbers g.

die' :: (RandomGen g) => Rand g Int
die' = getRandomR (1,6)
--The dice function uses replicate and sequence to simulate the roll of n dice.

dice :: (RandomGen g) => Int -> Rand g [Int]
dice n = sequence (replicate n die')
--To extract a value from the Rand monad, we can can use evalRandIO.

main = do
  values <- evalRandIO (dice 2)
  putStrLn (show values)
