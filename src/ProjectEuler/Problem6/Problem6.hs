module ProjectEuler.Problem6.Problem6 where

sumSquare :: [Integer] -> Integer
sumSquare = sum . map (^2)

squareSum :: [Integer] -> Integer
squareSum = (^2) . sum

main :: IO ()
main = do
    let l = [1..100]
    putStrLn $ show $ (squareSum l) - (sumSquare l)