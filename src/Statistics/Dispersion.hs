module Statistics.Dispersion where

import Statistics.Center

variance :: (Foldable t, Functor t, Fractional a) => t a -> a
variance vals = (sum $ fmap (\x -> x * x) deviations) / n
    where n = (fromIntegral $ length vals)
          mu = arithmeticMean vals
          deviations = fmap (\x -> x-mu) vals

stdev :: (Foldable t, Functor t, Floating a) => t a -> a
stdev vals = sqrt $ variance vals
