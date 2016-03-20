module MyGraph where

import qualified Data.Set as Set
import MyArray

type VIx = Int
tpye VData = [VIx]

type Graph = Array VIx VData

updateVertex :: VIx -> VData -> Graph -> Graph
updateVertex = update

dfs :: Graph -> [VIx]
dfsFrom :: Graph -> VIx -> Set -> Set
