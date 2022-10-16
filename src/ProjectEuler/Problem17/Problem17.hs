main = do 
  print $ countLetter 342
  print $ countLetter 115
  print $ sumList $ map countLetter [1..1000]

sumList :: [Int] -> Int
sumList [a] = a
sumList (a:as) = a + sumList as 

prefix :: Int -> Int
prefix 3 = 4
prefix 5 = 3
prefix 4 = 3
prefix 8 = 4
prefix a = countLetter a

countLetter :: Int -> Int
countLetter 0 = 0
countLetter 1 = 3
countLetter 2 = 3
countLetter 3 = 5
countLetter 4 = 4
countLetter 5 = 4
countLetter 6 = 3
countLetter 7 = 5
countLetter 8 = 5
countLetter 9 = 4
countLetter 10 = 3
countLetter 11 = 6
countLetter 12 = 6
countLetter 14 = 8
countLetter 20 = 6
countLetter 100 = 10
countLetter 1000 = 11
countLetter a | a < 20 = 4 + prefix (a-10)
              | a < 30 = 6 + countLetter (a `mod` 10)
              | a < 100 = prefix (quot a 10) + 2 + countLetter (a `mod` 10)
              | a `mod` 100 == 0 = countLetter (quot a 100) + 7
              | otherwise = countLetter (quot a 100) + 10 + countLetter (a `mod` 100)