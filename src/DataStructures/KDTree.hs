module DataStructures.KDTree where


data KDTree a = Empty | Node [a] (KDTree a) (KDTree a) deriving Show

-- Create a balanced k-d tree from a list of points
kdtree :: (Ord a) => [[a]] -> KDTree a
kdtree ps = kdtree' ps 0

-- Recursive helper function for kdtree, takes an extra parameter: depth
kdtree' :: (Ord a) => [[a]] -> Int -> KDTree a
kdtree' [] _     = Empty
kdtree' ps depth = Empty 