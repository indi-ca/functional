module Bursts where

import Control.Monad
import Control.Concurrent(threadDelay)

import Control.Monad.Random

import Data.List(sort)

-- if the time has elapsed
-- add the job to the job queue
-- the jobs execute immediately


--data Burst = Rand g [Float] | Rand g [Burst]
--    deriving (Show)

--data Burst g = Rand g [Float] | Rand g [Burst]
--    deriving (Show)



spark :: (RandomGen g) => Rand g Float
spark = getRandomR (-1.0, 1.0)

burst :: (RandomGen g) => Int -> Rand g [Float]
burst n = sequence (replicate n spark)

-- Create a burst of n items with a mean and a deviation
createBurst :: (RandomGen g) => Int -> Float -> Float -> Rand g [Float]
createBurst n mean deviation = fmap sort values
        where values = fmap (fmap (\x -> x * deviation + mean)) (burst n)


play = evalRandIO $ createBurst 5 10.0 3.0

main :: IO ()
main = forever $ do
    threadDelay 1000000
    putStrLn "do something"
