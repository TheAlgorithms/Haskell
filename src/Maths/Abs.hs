module Maths.Abs where

import Prelude hiding (abs)

-- The absolute value of a number greater than or equal to 0 is simply that number.
-- The absolute value of a number less than 0 is the negation of that number
abs :: (Num a, Ord a) => a -> a
abs n | n < 0 = negate n
      | otherwise = n
