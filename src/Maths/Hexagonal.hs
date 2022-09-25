module Maths.Hexagonal where

-- Infinite list of hexagonal numbers calculated with hn = 2n^2-n
hexagonal_numbers :: [Integer]
hexagonal_numbers = [x | n <- [1..], let x = (2*n*n-n)]

main :: IO()
main = do
    -- Print first 20 hexagonal numbers
    print $ take 20 hexagonal_numbers
