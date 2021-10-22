module ProjectEuler.Problem6.Problem6 where

square :: Integer -> Integer
square x = x * x

sumSquare :: [Integer] -> Integer
sumSquare = sum . map square

squareSum :: [Integer] -> Integer
squareSum = square . sum

main :: IO ()
main = do
    let l = [1..100]
    putStrLn $ show $ (squareSum l) - (sumSquare l)
