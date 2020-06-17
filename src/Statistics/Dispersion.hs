module Statistics.Dispersion where

import Statistics.Center

variance :: (Fractional a) => [a] -> a
variance vals = (sum $ zipWith (*) deviations deviations)/n
    where n = (fromIntegral $ length vals)
          mu = arithmeticMean vals
          deviations = map (\x -> x-mu) vals

stdev :: (Floating a) => [a] -> a
stdev vals = sqrt $ variance vals