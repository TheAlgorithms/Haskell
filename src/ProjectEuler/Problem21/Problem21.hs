main = do 
  print $ sumDivisors 220
  print $ sumDivisors 284
  print $ sumList $ filter amicable [1..10000]

amicable :: Int -> Bool
amicable a = ((sumDivisors $ sumDivisors a) == a) && (sumDivisors a /= a)

sumList :: [Int] -> Int
sumList [] = 0
sumList (a:as) = a + sumList as

sumDivisors :: Int -> Int
sumDivisors a = sumList $ filter modB [1..(a-1)]
  where modB b = (a `mod` b == 0)