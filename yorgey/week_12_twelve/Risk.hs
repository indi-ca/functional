{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List(sort)


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





sortedDice :: (RandomGen g) => Int -> Rand g [Int]
sortedDice n = fmap sort results
    where results = dice n

twoRandomSets :: (RandomGen g) => Int -> Int -> Rand g ([Int], [Int])
twoRandomSets x y = (sortedDice x) >>= \i1 ->
                    (sortedDice y) >>= \i2 ->
                    return (i1, i2)


-- All the True results are the attackers that won
carnage :: [Int] -> [Int] -> Battlefield
carnage x y = Battlefield att def
    where result = fmap (uncurry (>)) $ zip x y
          att = length (filter (\x -> x == True) result)
          def = length (filter (\x -> x == False) result)



battle :: Battlefield -> Rand StdGen Battlefield
battle bf = fmap (uncurry carnage) (twoRandomSets (attackers bf) (defenders bf))





battle_field = Battlefield 3 2
doit = evalRandIO $ battle battle_field


render :: IO Battlefield -> IO Int
render bf = fmap attackers bf

















