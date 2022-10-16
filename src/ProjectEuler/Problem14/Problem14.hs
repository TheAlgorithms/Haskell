main = do
  print $ solve [1..1000000]

solve :: [Int] -> [Int]
solve [a]    = path a
solve (x:xs)  | (head now) > (head after) = now
              | otherwise                 = after
              where after = solve xs
                    now = path x


path :: Int -> [Int]
path a = p a 1 a
  where p a n s | a == 1          = [n, s]
                | a `mod` 2 == 0  = p (quot a 2) (n+1) s
                | otherwise       = p (3*a+1) (n+1) s