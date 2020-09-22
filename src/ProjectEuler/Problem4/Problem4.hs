module ProjectEuler.Problem4.Problem4 where

isPalindrome :: Integer -> Bool
isPalindrome i = 
  let strInt = show i
      front = take (length strInt `div` 2) strInt 
      back = take (length strInt `div` 2)  $ reverse strInt
  in front == back

main = do
  print $ maximum [i * j | i <- [100..999], j <- [100..999], isPalindrome (i * j)]
