module ProjectEuler.Problem9.Problem9 where

-- Generate a list of natural number triples which sum to t
triples :: (Num a, Enum a) => a -> [(a,a,a)]
triples t = [(t-i-j, i, j) | i <- [1..t-1], j<- [1..t-i-1]]

-- Determine if a triple is pythagorean
isPythagorean :: (Num a, Eq a) => (a,a,a) -> Bool
isPythagorean (a,b,c) = a^2 + b^2 == c^2

-- Finds the pythagorean triple such that a + b + c = x; returns a * b *c
solution :: Int -> Maybe Int
solution x = let ts = filter isPythagorean (triples x)
                 in case ts of
                     [] -> Nothing
                     _  -> let (a,b,c) = head ts in Just (a * b * c)

main :: IO ()
main = do
    print (solution 1000)