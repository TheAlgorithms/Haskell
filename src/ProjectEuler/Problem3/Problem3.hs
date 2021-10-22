module ProjectEuler.Problem3.Problem3 where

largestPrimeFactor :: Integer -> Integer -> Integer
largestPrimeFactor divi val
  | val `mod` divi == 0 = if divi == val then val else largestPrimeFactor 2 $ val `div` divi
  | otherwise = largestPrimeFactor (divi + 1) val

main :: IO ()
main = print $ largestPrimeFactor 2 600851475143
