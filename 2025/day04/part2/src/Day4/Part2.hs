{-# LANGUAGE TupleSections #-}

module Day4.Part2
  ( solution
  ) where

import           Data.Array.IArray ( Array, (!), (!?), (//), array, indices )
import           Data.Maybe ( fromMaybe )

solution :: String -> Int
solution = countRemovableRolls . readRolls . lines

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

rolls :: Array (Int, Int) Int -> [(Int, Int)]
rolls a = filter (\i -> a ! i == 1) (indices a)

removeable :: Array (Int, Int) Int -> [(Int, Int)]
removeable a = filter (\i -> count a i < 4) (rolls a)

countAndRemove :: Array (Int, Int) Int -> (Int, Array (Int, Int) Int)
countAndRemove a =
  let r = removeable a
      c = length r
      a' = a // map (, 0) r
  in (c, a')

countRemovableRolls :: Array (Int, Int) Int -> Int
countRemovableRolls = count' 0

count' :: Int -> Array (Int, Int) Int -> Int
count' t a =
  let (c, a') = countAndRemove a
  in  if c == 0 then t else count' (t + c) a'
