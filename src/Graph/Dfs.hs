module Graph.Dfs where

import Data.List

type Node = Int
type Edge = (Node,Node)
type Graph = ([Node],[Edge])

depthfirst :: Graph -> Node -> [Node]
depthfirst (v,e) n
    | [x|x<-v,x==n] == [] = []
    | otherwise = dfrecursive (v,e) [n]
    
dfrecursive :: Graph -> [Node] -> [Node]
dfrecursive ([],_) _ = []
dfrecursive (_,_) [] = []
dfrecursive (v,e) (top:stack)
    | [x|x<-v,x==top] == [] = dfrecursive (newv, e) stack
    | otherwise = top : dfrecursive (newv, e) (adjacent ++ stack)
    where
        adjacent = [x | (x,y)<-e,y==top] ++ [x | (y,x)<-e,y==top]
        newv = [x|x<-v,x/=top]
        
connectedcomponents :: Graph -> [[Node]]
connectedcomponents ([],_) = []
connectedcomponents (top:v,e) 
    | remaining == [] = [connected]
    | otherwise = connected : connectedcomponents (remaining, e)
    where
        connected = depthfirst (top:v,e) top
        remaining = (top:v) \\ connected
        
        
dfsbipartite :: Graph -> [(Node, Int)] -> [Node] -> [Node] -> Bool
dfsbipartite ([],_) _ _ _ = True
dfsbipartite (_,_) [] _ _ = True
dfsbipartite (v,e) ((nv, 0):stack) odd even
    | [x|x<-v,x==nv] == [] = dfsbipartite (v, e) stack odd even
    | [] == intersect adjacent even = dfsbipartite (newv, e) ([(x,1)|x<-adjacent] ++ stack) odd (nv : even)
    | otherwise = False
    where
        adjacent = [x | (x,y)<-e,y==nv] ++ [x | (y,x)<-e,y==nv]
        newv = [x|x<-v,x/=nv]
dfsbipartite (v,e) ((nv, 1):stack) odd even
    | [x|x<-v,x==nv] == [] = dfsbipartite (v, e) stack odd even    
    | [] == intersect adjacent odd = dfsbipartite (newv, e) ([(x,0)|x<-adjacent] ++ stack) (nv : odd) even
    | otherwise = False
    where
        adjacent = [x | (x,y)<-e,y==nv] ++ [x | (y,x)<-e,y==nv]
        newv = [x|x<-v,x/=nv]

bipartite :: Graph -> Bool
bipartite ([],_) = True
bipartite (top:v,e) = dfsbipartite (top:v, e) [(top,0)] [] []
