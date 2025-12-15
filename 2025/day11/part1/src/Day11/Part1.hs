module Day11.Part1 where

import           AdventOfCode ( breakUpWith )
import qualified Data.Array as A
import qualified Data.Graph as G

solution :: String -> Int
solution s =
  let (graph, _nodeFromVertex, vertexFromKey) = readGraph s
  in  case mCountPaths graph vertexFromKey "you" "out" of
        Nothing -> error "No solution!"
        Just n -> n

mCountPaths ::
     G.Graph
  -> (String -> Maybe G.Vertex)
  -> String
  -> String
  -> Maybe Int
mCountPaths graph vertexFromKey src dst = do
  src' <- vertexFromKey src
  dst' <- vertexFromKey dst
  pure $ countPaths graph src' dst'

countPaths :: G.Graph -> G.Vertex -> G.Vertex -> Int
countPaths g src dst = paths A.! dst
 where
  bounds' :: (G.Vertex, G.Vertex)
  bounds' = A.bounds g

  topo :: [G.Vertex]
  topo = G.topSort g

  paths :: A.Array G.Vertex G.Vertex
  paths = foldl step initial topo

  initial :: A.Array G.Vertex G.Vertex
  initial = A.array bounds'
    [ (v, if v == src then 1 else 0) | v <- A.range bounds' ]

  step :: A.Array G.Vertex G.Vertex -> G.Vertex -> A.Array G.Vertex G.Vertex
  step arr u = arr A.// [ (v, arr A.! v + arr A.! u) | v <- g A.! u ]

readGraph ::
     String
  -> ( G.Graph
     , G.Vertex -> ((), String, [String])
     , String -> Maybe G.Vertex
     )
readGraph s =
  let ss = "out:" : lines s
  in  G.graphFromEdges $ map readLinks ss

readLinks :: String -> ((), String, [String])
readLinks s = case breakUpWith ' ' s of
  (v' : vs) ->
    let v = case reverse v' of
          (':' : rest) -> reverse rest
          _ -> error $ "readLinks: invalid label: " <> v'
    in  ((), v, vs)
  _ -> error $ "readlinks: invalid links: " <> s
