main = do 
  print $ (findLongFib !! 0) + 1

fibs :: [Integer] 
fibs = scanl (+) 1 (0:fibs)

isLarge :: Integer -> Bool
isLarge a = a >= 10^999

findLongFib :: [Int]
findLongFib = do
  a <- [1..]
  b <- [fibs !! a]
  if isLarge b then [a] else []