module Day12.Part1
  ( solution
  ) where

import           AdventOfCode ( breakUpWith )
import qualified Data.Array.Unboxed as UA
-- import           Data.Tuple.Extra ( first, second )
import qualified Data.List as L

type Shape = UA.UArray (Int, Int) Int
type Area = (Int, Int)
type Problem = (Area, [Int])

solution :: String -> Int
solution s =
  let (shapes, ps) = readSpec $ lines s
      vps = filter (viableProb shapes) ps
      num_vps = L.length vps
      ovps = filter obsViableProb vps
      num_ovps = L.length ovps
  in  if num_ovps == num_vps
        then num_ovps
        else undefined

size :: ((Int, Int), (Int, Int))
size = ((0, 0), (2, 2))

readSpec :: [String] -> ([Shape], [Problem])
readSpec ss = case breakUpWith "" ss of
  [ss0, ss1, ss2, ss3, ss4, ss5, ps] ->
    (map readShape [ss0, ss1, ss2, ss3, ss4, ss5], readProbs ps)
  _ -> error "readSpec: cannot parse specification!"

readShape :: [String] -> Shape
readShape [_ , s0 , s1 , s2] = toShape [s0, s1, s2]
readShape _ = error "readShape: cannot parse shape!"

toShape :: [[Char]] -> Shape
toShape css = UA.genArray size gen
 where
  gen (i, j) = case (css !! j) !! i of
    '#' -> 1
    '.' -> 0
    _ -> error "toShare: cannot parse cell!"

readProbs :: [String] -> [Problem]
readProbs = map readProb

readProb :: String -> Problem
readProb s = case breakUpWith ' ' s of
  (area : cs) -> (readArea area, map read cs)
  _ -> error "readProb: cannot parse problem!"

readArea :: String -> Area
readArea s = case L.unsnoc s of
  Just (s', ':') -> case breakUpWith 'x' s' of
    [w, h] -> (read w, read h)
    _ -> err
  _ -> err
 where
  err = error "readArea: cannot parse area!"

obsViableProb :: Problem -> Bool
obsViableProb ((w, h), ps) = (w `div` 3) * (h `div` 3) >= L.sum ps

viableProb :: [Shape] -> Problem -> Bool
viableProb shapes ((w, h), ps) =
  let cs = countShapes shapes
  in  w * h > L.sum (L.zipWith (*) cs ps)

countShapes :: [Shape] -> [Int]
countShapes = map countShape

countShape :: Shape -> Int
countShape = UA.foldlArray' (+) 0

{-
normArea :: Area -> Area
normArea (w, h) = if w >=h then (w, h) else (h, w)

normProb :: Problem -> Problem
normProb = first normArea

normSpec :: ([Shape], [Problem]) -> ([Shape], [Problem])
normSpec = second (map normProb)

sortProbs :: [Problem] -> [Problem]
sortProbs = L.sortOn fst

rotShape :: Shape -> Shape
rotShape shape = UA.genArray size gen
 where
  gen (i, j) = shape UA.! (2 - j, i)

flipHShape :: Shape -> Shape
flipHShape shape = UA.genArray size gen
 where
  gen (i, j) = shape UA.! (2 - i, j)

flipVShape :: Shape -> Shape
flipVShape shape = UA.genArray size gen
 where
  gen (i, j) = shape UA.! (i, 2 - j)

mkShapes :: Shape -> [Shape]
mkShapes shape =
  let shape1 = rotShape shape
      shape2 = rotShape shape1
      shape3 = rotShape shape2
      shapes = [shape, shape1, shape2, shape3]
      flipHShapes = map flipHShape shapes
      flipVShapes = map flipVShape shapes
  in L.nub $ shapes <> flipHShapes <> flipVShapes
-}
