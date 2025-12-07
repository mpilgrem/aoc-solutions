module Day4.Part1
  ( solution
  ) where

import           Data.Array.IArray ( Array, (!), (!?), array, indices )
import           Data.Maybe ( fromMaybe )

solution :: String -> Int
solution = totalCount . readRolls . lines

readRolls :: [String] -> Array (Int, Int) Int
readRolls ss = array size $ concatMap readShelf $ zip [0 ..] ss
 where
  size = ((0, 0), (w - 1, h - 1))
  h = length ss
  w = case ss of
        [] -> error "There are no shelves of rolls!"
        (s : _) -> length s

toInt :: Char -> Int
toInt '.' = 0
toInt '@' = 1
toInt c = error $ "Unrecognised cell: " <> [c]

readShelf :: (Int, String) -> [((Int, Int), Int)]
readShelf (v, s) = zipWith mkCell s [0 ..]
 where
  mkCell c i = ((i, v), toInt c)

dirs :: [(Int, Int)]
dirs =
  [ (x, y)
  | x <- [-1, 0, 1]
  , y <- [-1, 0, 1]
  , x /= 0 || y /= 0
  ]

cell :: Array (Int, Int) Int -> (Int, Int) -> Int
cell a p = fromMaybe 0 (a !? p)

count :: Array (Int, Int) Int -> (Int, Int) -> Int
count a p = sum $ map (cell a . (`add` p)) dirs

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (dx, dy) (x, y)  = (x + dx, y + dy)

totalCount :: Array (Int, Int) Int -> Int
totalCount a = length $ filter (< 4) $ map (count a) $
  filter (\i -> a ! i == 1) (indices a)
