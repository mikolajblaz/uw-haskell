module MyGraph(
  Graph,
  fromList, updateVertex, neighbours, dfs, dfsFrom
) where

import qualified Data.Set as Set
import qualified MyArray

type VIx = Int
type VData = [VIx]
type Graph = MyArray.Array VIx VData

fromList :: [(VIx, VData)] -> Graph
fromList = MyArray.array (0::VIx, maxBound::VIx)

updateVertex :: VIx -> VData -> Graph -> Graph
updateVertex = MyArray.update

neighbours :: Graph -> VIx -> [VIx]
neighbours = (MyArray.!)

dfs :: Graph -> [VIx]
dfs g = Set.toAscList $ dfsFrom g 1 Set.empty

dfsFrom :: Graph -> VIx -> Set.Set VIx -> Set.Set VIx
dfsFrom g v visited = foldr action visited $ neighbours g v
  where
    action neigh visited | neigh `Set.member` visited = visited
                         | otherwise = dfsFrom g neigh (Set.insert neigh visited)
