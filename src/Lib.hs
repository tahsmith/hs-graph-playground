module Lib
    ( Graph(..)
    , bfs
    ) where

data Label = Unvisited | Visited Int deriving (Eq, Show)
newtype Labelling = Labelling { unLabelling :: [Label] } deriving (Eq, Show)

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

relabel :: Labelling -> Int -> Int -> Labelling
relabel labelling v n = Labelling newLabels where
    labels = unLabelling labelling
    newLabels = slice 0 (v - 1) labels ++ [Visited n] ++ slice (v + 1) (length labels) labels 

(Labelling labels) !!! v = labels !! v
    

newtype Graph = Graph [[Int]]
adjList :: Graph -> [[Int]]
adjList (Graph g) = g

neighbours :: Graph -> Int -> [Int]
neighbours (Graph g)  = (!!) g

 
visit :: Graph -> Labelling -> Int -> [Int] -> Labelling
visit _ labels _ [] = labels
visit g labels i queue = visit g labels' i' queue'
    where
        (v:vs) = queue
        labels' = relabel labels v i
        queue' = vs ++ filter ((==) Unvisited . (!!!) (labels')) (neighbours g v)
        i' = i + 1
 
bfs :: Graph -> Int -> Labelling
bfs g v = visit g initialLabels 0 initialQueue 
    where
        initialLabels = Labelling [Unvisited | _ <- [1..(length $ adjList g)]]
        initialQueue = [v]


