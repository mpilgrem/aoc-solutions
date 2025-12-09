module Day8.Part2
  ( solution
  ) where

import           AdventOfCode ( breakUpWith )
import           Control.Monad ( forM_ )
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

solution :: String -> Int
solution s =
  let junctions = readJunctions $ lines s
      n = V.length junctions
      (i, j) = gatherUntil n $ sortOnDist $ junctionDists $ junctions
      (xi, _, _) = junctions V.! i
      (xj, _, _) = junctions V.! j
  in  xi * xj

readJunctions :: [String] -> V.Vector (Int, Int, Int)
readJunctions = V.fromList . map readJunction

readJunction :: String -> (Int, Int, Int)
readJunction s = case breakUpWith ',' s of
  [x, y, z] -> (read x, read y, read z)
  _ -> error $ "Cannot parse junction location: " <> s

junctionDists :: V.Vector (Int, Int, Int) -> V.Vector (Int, (Int, Int))
junctionDists junctions = V.create $ do
  vm <- VM.new size
  forM_ [0 .. n - 2] $ \i -> do
    forM_ [i + 1 .. n - 1] $ \j -> do
      VM.write vm (index n i j) (junctionDist i j, (i, j))
  pure vm
 where
  n = V.length junctions
  size = (n - 1) * n `div` 2
  junctionDist i j =
    let (xi, yi, zi) = junctions V.! i
        (xj, yj, zj) = junctions V.! j
        (dx, dy, dz) = (xi - xj, yi - yj, zi - zj)
    in dx * dx + dy * dy + dz * dz

index :: Int -> Int -> Int -> Int
index n i j = (2 * n - 1 - i) * i `div` 2 + j - i - 1

sortOnDist :: V.Vector (Int, (Int, Int)) -> [(Int, Int)]
sortOnDist = map snd . L.sortOn fst . V.toList

gatherUntil :: Int -> [(Int, Int)] -> (Int, Int)
gatherUntil n pairs = gatherUntil' [] pairs
 where
  gatherUntil' :: [S.Set Int] -> [(Int, Int)] -> (Int, Int)
  gatherUntil' _ [] = error $ "No solution!"
  gatherUntil' _ [pair] = pair
  gatherUntil' ss (pair : rest) =
    let ss' = gather ss pair
    in  case ss' of
          [s] -> if S.size s == n
                   then pair
                   else gatherUntil' ss' rest
          _ -> gatherUntil' ss' rest

gather :: [S.Set Int] -> (Int, Int) -> [S.Set Int]
gather ss (i, j) =
  let (inSs, outSs ) = L.partition has ss
  in  (S.unions (S.fromAscList [i, j] : inSs)) : outSs
 where
  has :: S.Set Int -> Bool
  has s = i `S.member` s || j `S.member` s
