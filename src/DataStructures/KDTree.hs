module DataStructures.KDTree where
import Data.List (sort)

data KDTree a = Empty | Node [a] (KDTree a) (KDTree a) deriving Show

-- Point type where an axis is selected, for sorting points
data Point a = P [a] Int deriving Eq
instance (Ord a) => Ord (Point a) where
    (P p1 k1) <= (P p2 k2) = p1!!k1 <= p2!!k2

-- Create a balanced k-d tree from a list of points
kdtree :: (Ord a) => [[a]] -> KDTree a
kdtree []     = Empty
kdtree (p:ps) = kdtree' (p:ps) (length p) 0

-- Recursive helper function for kdtree, takes extra parameters k and depth
kdtree' :: (Ord a) => [[a]] -> Int -> Int -> KDTree a
kdtree' [] _ _     = Empty
kdtree' ps k depth = let axis = (depth `mod` k)
                         sps = pointSort ps axis
                         m = (length sps) `div` 2
                         l = take m sps
                         r = drop (m+1) sps
                         in Node (sps!!m) (kdtree' l k (depth+1)) (kdtree' r k (depth + 1))

-- Sort a list of points with respect to an axis
pointSort :: (Ord a) => [[a]] -> Int -> [[a]]
pointSort [] _ = []
pointSort [x] _ = [x]
pointSort ps axis = map (\(P p _) -> p) (sort (map (`P` axis) ps))

-- Add a point to a k-d tree
addPoint :: (Ord a) => [a] -> KDTree a -> KDTree a
addPoint p tree = addPoint' p tree 0 (length p)

-- Recursive helper function for addPoint
addPoint' :: (Ord a) => [a] -> KDTree a -> Int -> Int -> KDTree a
addPoint' p Empty _ _ = Node p Empty Empty
addPoint' p (Node root l r) d k
    | P p axis < P root axis = Node root (addPoint' p l (d+1) k) r
    | otherwise = Node root l (addPoint' p r (d+1) k)
    where axis = d `mod` k

-- Remove a point from a k-d tree
-- First, traverse the tree until the targeted point is found. Then, replace this node with a new tree,
-- formed from the set of all children of the removed node.
removePoint :: (Ord a) => [a] -> KDTree a -> KDTree a
removePoint p tree = removePoint' p tree 0 (length p)

-- Recursive helper function for removePoint
removePoint' :: (Ord a) => [a] -> KDTree a -> Int -> Int -> KDTree a
removePoint' _ Empty _ _ = Empty
removePoint' p (Node root l r) d k
    | p == root = kdtree (pointSet l ++ pointSet r)
    | P p axis < P root axis = Node root (removePoint' p l (d+1) k) r
    | otherwise =  Node root l (removePoint' p r (d+1) k)
    where axis = d `mod` k

-- Given a k-d tree, return the set of all points on the tree
pointSet :: (Ord a) => KDTree a -> [[a]]
pointSet Empty = []
pointSet (Node root l r) = root : pointSet l ++ pointSet r

-- Find the point on the given k-d tree which is closest to the given point
-- Return nothing if the tree is empty
nnSearch :: (Num a, Ord a) => [a] -> KDTree a -> Maybe [a]
nnSearch p tree = nnSearch' p tree 0 (length p)

-- Recursive helper function for nnSearch
nnSearch' :: (Num a, Ord a) => [a] -> KDTree a -> Int -> Int -> Maybe [a]
nnSearch' _ Empty _ _ = Nothing
nnSearch' _ (Node root Empty Empty) _ _ = Just root
nnSearch' p (Node root l r) d k = 
    let axis = d `mod` k
        (next, other) = if P p axis < P root axis then (l, r) else (r, l)
        nextBest = nnSearch' p next (d+1) k
        best = case nextBest of
            Just nb -> if sqrDist p nb < sqrDist p root then nb else root
            Nothing -> root
        otherBest = 
            if (p!!axis - root!!axis) * (p!!axis - root!!axis) < sqrDist p best then 
                nnSearch' p other (d+1) k 
            else 
                Nothing
        in case otherBest of
            Just ob -> if sqrDist p ob < sqrDist p best then Just ob else Just best
            Nothing -> Just best

-- Compute squared distance between two points
sqrDist :: (Num a) => [a] -> [a] -> a
sqrDist x y = sum [(b - a) * (b - a) | (a,b) <- zip x y]
