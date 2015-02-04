module Bursts where

import Control.Monad
import Control.Concurrent(threadDelay)
import Control.Monad.Random(Rand, RandomGen, evalRandIO, getRandomR)

import Data.List(sort)




--data Burst g = Rand g [Float] | Rand g [Burst]
--    deriving (Show)

--data Burst g = Rand g [Float] | Rand g [Burst]
--    deriving (Show)

--data Burst = Burst Float | Burst Int
--    deriving (Show)


spark :: (RandomGen g) => Rand g Float
spark = getRandomR (-1.0, 1.0)

burst :: (RandomGen g) => Int -> Rand g [Float]
burst n = sequence (replicate n spark)

-- Create a burst of n items with a mean and a deviation
createBurst :: (RandomGen g) => Int -> Float -> Float -> Rand g [Float]
createBurst n mean deviation = fmap sort values
        where values = fmap (fmap (\x -> x * deviation + mean)) (burst n)


-- I want to create a burst of bursts
-- So, the number, then the mean of the burst, and yes, perhaps the deviation
-- now, ideally, I yield something that creates more bursts
-- however, this is something that diverges
-- so, I do have to provide a depth
-- but, now, there is something else that I need
-- I need the deviation of the mini burst
-- I could make it a function of the original deviation

-- Finally, I need to collect the values and sort them

createBursts :: (RandomGen g) => Int -> Float -> Float -> Rand g [Float]
createBursts n mean deviation = base_burst
    where base_burst = createBurst n mean deviation
          new_deviation = deviation / 10



play = evalRandIO $ createBurst 5 10.0 3.0

main :: IO ()
main = forever $ do
    threadDelay 1000000
    putStrLn "do something"
