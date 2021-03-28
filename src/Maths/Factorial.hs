module Maths.Factorial where

fac :: Integer -> Integer 
fac 0 = 1
fac n = n * fac (n - 1)

main :: IO ()
main = do
    print (fac 4)