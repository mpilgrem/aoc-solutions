module Day8.Part1
  ( solution
  ) where

import           AdventOfCode ( breakUpWith )
import           Control.Monad ( forM_ )
import qualified Data.List as L
import           Data.Ord ( Down (..) )
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

solution :: Int -> String -> Int
solution n
  = L.product
  . map S.size
  . take 3
  . sortOnCount
  . gather
  . take n
  . sortOnDist
  . junctionDists
  . readJunctions
  . lines

readJunctions :: [String] -> V.Vector (Int, Int, Int)
readJunctions = V.fromList . map readJunction

readJunction :: String -> (Int, Int, Int)
readJunction s = case breakUpWith ',' s of
  [x, y, z] -> (read x, read y, read z)
  _ -> error $ "Cannot parse junction location: " <> s

junctionDists :: V.Vector (Int, Int, Int) -> V.Vector (Int, (Int, Int))
junctionDists v = V.create $ do
  vm <- VM.new size
  forM_ [0 .. n - 2] $ \i -> do
    forM_ [i + 1 .. n - 1] $ \j -> do
      VM.write vm (index n i j) (junctionDist i j, (i, j))
  pure vm
 where
  n = V.length v
  size = (n - 1) * n `div` 2
  junctionDist i j =
    let (x1, y1, z1) = v V.! i
        (x2, y2, z2) = v V.! j
        (dx, dy, dz) = (x1 - x2, y1 - y2, z1 - z2)
    in dx * dx + dy * dy + dz * dz

index :: Int -> Int -> Int -> Int
index n i j = (2 * n - 1 - i) * i `div` 2 + j - i - 1

sortOnDist :: V.Vector (Int, (Int, Int)) -> [(Int, Int)]
sortOnDist = map snd . L.sortOn fst . V.toList

gather :: [(Int, Int)] -> [S.Set Int]
gather is = foldl' gather' [] is

gather' :: [S.Set Int] -> (Int, Int) -> [S.Set Int]
gather' ss (i, j) =
  let (inSs, outSs ) = L.partition has ss
  in  (S.unions (S.fromAscList [i, j] : inSs)) : outSs
 where
  has :: S.Set Int -> Bool
  has s = i `S.member` s || j `S.member` s

sortOnCount :: [S.Set Int] -> [S.Set Int]
sortOnCount = L.sortOn (Down . S.size)
