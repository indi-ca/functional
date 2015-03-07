module Stats where

import Statistics.Sample
import Data.Vector
import qualified Statistics.Distribution as D
import Statistics.Distribution.Normal

--sample = fromList [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]


source_sample = [1, 2, 3, 4, 50, 50, 4, 3, 2, 1]

sample = fromList source_sample
weightedSample = fromList [(1, 10), (2, 1), (3, 1), (4, 1), (5, 1)]



dist = normalDistr 10 5






main = do
    print sample
    print $ variance sample
    print $ fastVariance sample
    print $ varianceWeighted weightedSample
    print $ varianceUnbiased sample
    print $ "fastVarianceUnbiased: " Prelude.++ (show $ fastVarianceUnbiased sample)
    print $ meanVariance sample
    print $ meanVarianceUnb sample

    print weightedSample

    print $ range sample

    print $ mean sample
    print $ meanWeighted weightedSample
    print $ harmonicMean sample
    print $ geometricMean sample


    print $ stdDev sample
    print $ fastStdDev sample

    print $ standard
    print $ normalDistr 10 5

    print $ D.cumulative standard 0
    print $ D.quantile standard 0.5
    print $ D.density standard 0
    print $ D.mean standard
    print $ D.variance standard
    print $ D.stdDev standard

    print $ skewness sample
    print $ kurtosis sample




make_m m x k = m + (x - m) / k
make_s m x s old_m = s + (x - m) * (x - old_m)


-- sample, k, m, s
-- and finally we return s
-- k starts at 1
variance' :: [Double] -> Int -> Double -> Double -> Double
variance' [] k m s = s
variance' (x:xs) k m s = variance' xs k' m'' s'
    where m' = m
          m'' = m + (x - m) / (fromIntegral k :: Double)
          s' = s + (x - m) * (x - m')
          k' = k + 1

determineVariance :: [Double] -> Double
determineVariance [] = 0
determineVariance xs = s / (fromIntegral (Prelude.length xs) :: Double) -- Don't think I need to subtract one in here
    where s = variance' xs 1 0 0


