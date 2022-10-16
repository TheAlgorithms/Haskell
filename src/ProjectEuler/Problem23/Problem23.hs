import Data.Set (Set)
import qualified Data.Set as Set

main = do 
  print $ length abundants
  print $ sum $ filter notSumOfTwoAbun [1..30000]

notSumOfTwoAbun :: Int -> Bool
notSumOfTwoAbun a = notSum' a abundants

notSum' :: Int -> [Int] -> Bool
notSum' a [] = True
notSum' a (x:xs)  | Set.member (a-x) abundantSet = False
                  | otherwise = notSum' a xs

abundantSet :: Set Int
abundantSet = Set.fromList abundants

abundants :: [Int] 
abundants = filter isAbundant [1..30000]

isAbundant :: Int -> Bool 
isAbundant a = sumDivisors a > a

sumDivisors :: Int -> Int
sumDivisors a = sum $ filter (\b -> (a `mod` b) == 0) [1..(a-1)]