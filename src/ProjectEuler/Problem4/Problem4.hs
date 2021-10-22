module ProjectEuler.Problem4.Problem4 where

isPalindrome :: Integer -> Bool
isPalindrome i = strInt == reverse strInt where strInt = show i

main :: IO ()
main = print $ maximum [i * j | i <- [100..999], j <- [100..999], isPalindrome (i * j)]
