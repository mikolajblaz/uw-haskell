module MyGraph(
  Graph,
  graphFromList, updateVertex, neighbours, dfs, dfsFrom
) where

import qualified Data.Set as Set
import qualified MyArray

type VIx = Int
type VData = [VIx]
type Graph = MyArray.Array VIx VData

graphFromList :: [(VIx, VData)] -> Graph
graphFromList = MyArray.array (0::VIx, maxBound::VIx)

updateVertex :: VIx -> VData -> Graph -> Graph
updateVertex = MyArray.update

isVertex :: VIx -> Graph -> Bool
isVertex = MyArray.present

neighbours :: Graph -> VIx -> [VIx]
neighbours = (MyArray.!)

dfs :: Graph -> [VIx]
dfs g | 1 `isVertex` g = Set.toAscList $ dfsFrom g 1 (Set.singleton 1)
      | otherwise = []

-- | Właściwa realizacja algorytmu DFS.
-- | Dla każdego nieodwiedzonego sąsiada wywołuje rekurencyjnie siebie.
dfsFrom :: Graph -> VIx -> Set.Set VIx -> Set.Set VIx
dfsFrom g v visited = foldr action visited $ neighbours g v
  where
    action neigh visited | neigh `Set.member` visited = visited
                         | otherwise = dfsFrom g neigh (Set.insert neigh visited)
