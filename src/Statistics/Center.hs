module Statistics.Center where

import qualified Data.Sort as S
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Algorithms.Intro as VGAI

-- Measures of central tendency.

arithmeticMean :: (Foldable t, Fractional a) => t a -> a
arithmeticMean vals = (sum vals) / (fromIntegral $ length vals)

geometricMean :: (Foldable t, Floating a) => t a -> a
geometricMean vals = (product vals) ** (1/(fromIntegral $ length vals))

harmonicMean :: (Foldable t, Functor t, Fractional a) => t a -> a
harmonicMean vals = (sum $ fmap (1/) vals) / (fromIntegral $ length vals)

-- For median, since the containers are sorted differently, we need to use
-- different methods

medianList :: (Fractional a, Ord a) => [a] -> a
medianList = medianListSorted . S.sort

medianVector
  :: (VG.Vector vector a, Foldable vector, Fractional a, Ord a)
  => vector a -> a
medianVector = medianVectorSorted . VG.modify VGAI.sort

-- When sorted, the two median algorithms are quite similar. We can reduce
-- duplication by adding an export list to the module and using more
-- higher-order functions but let's leave this for a different PR.

medianListSorted :: Fractional a => [a] -> a
medianListSorted vals
  | odd n = vals !! mid
  | otherwise = arithmeticMean $ take 2 $ drop (mid - 1) vals
  where
    n = length vals
    mid = n `div` 2

medianVectorSorted
  :: (VG.Vector vector a, Foldable vector, Fractional a)
  => vector a -> a
medianVectorSorted vals
  | odd n = vals VG.! mid
  | otherwise = arithmeticMean $ VG.take 2 $ VG.drop (mid - 1) vals
  where
    n = length vals
    mid = n `div` 2
