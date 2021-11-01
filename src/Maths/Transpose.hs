module Maths.Transpose where

-- Return the transpose of a matrix
transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

main = do
    print (transpose [[1, 2, 3], [4, 5, 6]])