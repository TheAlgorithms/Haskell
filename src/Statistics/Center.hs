module Statistics.Center where

import qualified Data.Sort as S

-- Measures of central tendency.

arithmeticMean :: (Fractional a) => [a] -> a
arithmeticMean vals = (sum vals)/(fromIntegral $ length vals)

geometricMean :: (Floating a) => [a] -> a
geometricMean vals = (product vals) ** (1/(fromIntegral $ length vals))

harmonicMean :: (Fractional a) => [a] -> a
harmonicMean vals = (sum $ map (1/) vals)/(fromIntegral $ length vals)

median :: (Fractional a, Ord a) => [a] -> a
median vals = if odd n 
              then head $ drop mid sortedVals 
              else arithmeticMean $ take 2 $ drop (mid-1) sortedVals
    where sortedVals = (S.sort vals)
          n = length vals
          mid = n `div` 2 