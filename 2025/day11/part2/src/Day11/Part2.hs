module Day11.Part2 where

import           AdventOfCode ( breakUpWith )
import           Control.Monad ( forM_ )
import           Control.Monad.ST ( ST, runST )
import qualified Data.Array as A
import qualified Data.Array.MArray as MA
import qualified Data.Array.ST as STA
import qualified Data.Graph as G

solution :: String -> Int
solution s =
  let (graph, _nodeFromVertex, vertexFromKey) = readGraph s
  in  case mCountAllValidPaths graph vertexFromKey of
        Nothing -> error "No solution!"
        Just n -> n

mCountAllValidPaths ::
     G.Graph
  -> (String -> Maybe G.Vertex)
  -> Maybe Int
mCountAllValidPaths graph vertexFromKey = do
  svr <- vertexFromKey "svr"
  dac <- vertexFromKey "dac"
  fft <- vertexFromKey "fft"
  out <- vertexFromKey "out"
  let dacToFft = countPaths graph dac fft
      (n1, n2, n3) = if dacToFft == 0
        then
          ( countPaths graph svr fft
          , countPaths graph fft dac
          , countPaths graph dac out
          )
        else
          ( countPaths graph svr dac
          , dacToFft
          , countPaths graph fft out
          )
    in pure $ n1 * n2 * n3

countPaths :: G.Graph -> G.Vertex -> G.Vertex -> Int
countPaths g src dst = runST $ do
  let (vl, vu) = A.bounds g
  dp <- MA.newArray (vl, vu) 0 :: ST s (STA.STUArray s G.Vertex Int)
  MA.writeArray dp src 1
  forM_ (G.topSort g) $ \u -> do
    pu <- MA.readArray dp u
    forM_ (g A.! u) $ \v ->
      MA.modifyArray dp v (+ pu)
  MA.readArray dp dst

readGraph ::
     String
  -> (G.Graph, G.Vertex -> ((), String, [String]), String -> Maybe G.Vertex)
readGraph s =
  let ss = "out:" : lines s
  in  G.graphFromEdges $ map readLinks ss

readLinks :: String -> ((), String, [String])
readLinks s = case breakUpWith ' ' s of
  (v' : vs) ->
    let v = case reverse v' of
          (':' : rest) -> reverse rest
          _ -> error $ "readLinks: invalid label: " <> v'
    in ((), v, vs)
  _ -> error $ "readlinks: invalid links: " <> s
