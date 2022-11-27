module Util.BFS where

import Util.Util

bfs :: Eq a => (Integer -> [a] -> [a]) -> (a -> Bool) -> (a -> [a]) -> [a] -> [[a]]
bfs trace stopCondition findNeighbors startNodes = runBFS 0 [] startNodes
    where
        runBFS i _ [] = [trace i []]
        runBFS i old new
            | any stopCondition (trace i new) = [new]
            | otherwise                       = runBFS (succ i) old' new'
            where
                neighbors = new >>= findNeighbors
                old'      = old ++ new
                new'      = neighbors `without` old'
