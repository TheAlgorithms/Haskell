main = do
  print $ length $ unique numbers

numbers :: [Integer]
numbers = [a^b | a <- [2..100], b <- [2..100]]

contain :: [Integer] -> Integer -> Bool
contain [] x = False
contain (a:as) b  | a == b    = True
                  | otherwise = contain as b

unique :: [Integer] -> [Integer]
unique [] = []
unique (a:as) | contain as a  = unique as
              | otherwise     = a:(unique as)