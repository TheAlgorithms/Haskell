main = do 
  print $ solve 1

-- expected answer around 600ish, actually 8000ish
solve :: Integer -> Integer
solve a 
  | (length $ divisors $ triangular a) >= 500 = triangular a
  | otherwise = solve (a+1)

triangular :: Integer -> Integer
triangular a = quot (a*(a+1)) 2

divisors :: Integer -> [Integer]
divisors num = divisors' 2 num 
  where 
    divisors'  n k  | n*n == k        = [n, k]
                    | n*n > k         = [k]
                    | k `mod` n == 0  = (n:(div k n): (divisors' (n+1) k))
                    | otherwise       = divisors' (n+1) k