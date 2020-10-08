module SpecializedStructure.MergeFindSet where

import qualified Data.Map as Map
import qualified Data.Set as Set

type Mfset a = (Map.Map a a, Map.Map a Int)

fromSet :: Ord a => Set.Set a -> Mfset a
fromSet ns = (fathers, ranks)
    where
        fathers = Map.fromList [(i, i) | i <- Set.toList ns]
        ranks   = Map.fromList [(i, 0) | i <- Set.toList ns]

find :: Ord a => a -> Mfset a -> a
find x mfset = let father = fst mfset Map.! x
               in if father == x then x else find father mfset

merge :: Ord a => a -> a -> Mfset a -> Mfset a
merge x y mfset
    | x' == y'  = mfset
    | rx == ry  = (x' `connectTo` y', Map.adjust (+1) y' ranks)
    | rx < ry   = (x' `connectTo` y', ranks)
    | otherwise = (y' `connectTo` x', ranks)
    where
        (fathers, ranks) = mfset
        connectTo a b = Map.adjust (const b) a fathers
        findWithRank a = (\f -> (f, ranks Map.! f)) $ find a mfset
        (x', rx) = findWithRank x
        (y', ry) = findWithRank y
