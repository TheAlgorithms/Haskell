{-# LANGUAGE TupleSections #-}
module Graph.DirectedGraph where

import qualified Data.Set as Set
import qualified Data.Map as Map

type Graph a = Map.Map a (Set.Set a)

empty :: Ord a => Graph a
empty = Map.empty

insertNode :: Ord a => a -> Graph a -> Graph a
insertNode u g
    | Map.member u g = g
    | otherwise      = Map.insert u Set.empty g

insertEdge :: Ord a => (a, a) -> Graph a -> Graph a
insertEdge (u, v) = Map.insertWith Set.union u (Set.singleton v) . insertNode u . insertNode v

deleteNode :: Ord a => a -> Graph a -> Graph a
deleteNode u = Map.delete u . Map.map (Set.delete u)

adjacentNodes :: Ord a => a -> Graph a -> Maybe (Set.Set a)
adjacentNodes = Map.lookup

nodes :: Ord a => Graph a -> Set.Set a
nodes = Map.keysSet

edges :: Ord a => Graph a -> [(a, a)]
edges = concat . Map.elems . Map.mapWithKey (\u -> map (u,) . Set.toList)

-- Creates graph from list of edges
fromList :: Ord a => [(a, a)] -> Graph a
fromList = foldr insertEdge Map.empty

-- Creates new graph by flipping every edge
transpose :: Ord a => Graph a -> Graph a
transpose = fromList . map (\(a, b)->(b, a)) . edges
