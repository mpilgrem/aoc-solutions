module Day9.Part1
  ( solution
  ) where

import           AdventOfCode ( breakUpWith )
import           Control.Monad ( forM_ )
import qualified Data.List as L
import           Data.Ord ( Down (..) )
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

solution :: String -> Int
solution s = case sortOnArea $ rectAreas $ readTiles $ lines s of
  [] -> error "No solution"
  (a : _) -> a

readTiles :: [String] -> V.Vector (Int, Int)
readTiles = V.fromList . map readTile

readTile :: String -> (Int, Int)
readTile s = case breakUpWith ',' s of
  [x, y] -> (read x, read y)
  _ -> error $ "Cannot parse junction location: " <> s

rectAreas :: V.Vector (Int, Int) -> V.Vector (Int, (Int, Int))
rectAreas tiles = V.create $ do
  vm <- VM.new size
  forM_ [0 .. n - 2] $ \i -> do
    forM_ [i + 1 .. n - 1] $ \j -> do
      VM.write vm (index i j) (rectArea i j, (i, j))
  pure vm
 where
  n = V.length tiles
  size = (n - 1) * n `div` 2

  rectArea i j =
    let (xi, yi) = tiles V.! i
        (xj, yj) = tiles V.! j
        (w, h) = (abs (xi - xj + 1), abs (yi - yj + 1))
    in w * h

  index :: Int -> Int -> Int
  index i j = (2 * n - 1 - i) * i `div` 2 + j - i - 1

sortOnArea :: V.Vector (Int, (Int, Int)) -> [Int]
sortOnArea = map fst . L.sortOn (Down . fst) . V.toList
