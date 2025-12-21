module Day9.Part2
  ( solution
  ) where

import           AdventOfCode ( breakUpWith )
import           Control.Monad ( forM_ )
import qualified Data.List as L
import           Data.Ord ( Down (..) )
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

type Loc = (Int, Int)
type Index = Int
type Rect = (Loc, Loc)
type Edge = (Loc, Loc)

-- | Type synonym representing normalised horizontal or vertical edges
type DEdge = (Int, Int, Int)

solution :: String -> Int
solution s =
  let tiles = readTiles $ lines s
      (hDEdges, vDEdges) = partitionEdges $ edges tiles
      areas = rectAreas tiles
      areas' = V.filter (test . snd) areas
      test (i, j) = isFull hDEdges vDEdges (tiles V.! i, tiles V.! j)
  in  case sortOnArea areas' of
        [] -> error "No solution"
        (a : _) -> a

readTiles :: [String] -> V.Vector Loc
readTiles = V.fromList . map readTile

readTile :: String -> Loc
readTile s = case breakUpWith ',' s of
  [x, y] -> (read x, read y)
  _ -> error $ "Cannot parse junction location: " <> s

rectAreas :: V.Vector Loc -> V.Vector (Int, (Index, Index))
rectAreas tiles = V.create $ do
  vm <- VM.new size
  forM_ [0 .. n - 2] $ \i -> do
    forM_ [i + 1 .. n - 1] $ \j -> do
      VM.write vm (index i j) (rectArea i j, (i, j))
  pure vm
 where
  n = V.length tiles
  size = (n - 1) * n `div` 2

  rectArea :: Index -> Index -> Int
  rectArea i j =
      let (xi, yi) = tiles V.! i
          (xj, yj) = tiles V.! j
          (w, h) = (abs (xi - xj) + 1, abs (yi - yj) + 1)
      in w * h

  index :: Int -> Int -> Int
  index i j = (2 * n - 1 - i) * i `div` 2 + j - i - 1

sortOnArea :: V.Vector (Int, (Int, Int)) -> [Int]
sortOnArea = map fst . L.sortOn (Down . fst) . V.toList

normRect :: Rect -> Rect
normRect ((xi, yi), (xj, yj))
  | xj >= xi && yj >= yi = ((xi, yi), (xj, yj))
  | xi >= xj && yi >= yj = ((xj, yj), (xi, yi))
  | xj >= xi && yi >= yj = ((xi, yj), (xj, yi))
  | xi >= xj && yj >= yi = ((xj, yi), (xi, yj))
  | otherwise = error "The impossible happened!"

normEdge :: Edge -> Edge
-- We can reuse the code for normalising a rectangle:
normEdge = normRect

-- Assumes that both edges are normalised and orthogonal.
edgeCross :: DEdge -> DEdge -> Bool
edgeCross (c1, c1i, c1j) (c2, c2i, c2j) =
  c2 > c1i && c2 < c1j && c1 > c2i && c1 < c2j

-- Normalised edges, including the starting tile
edges :: V.Vector Loc -> V.Vector Edge
edges tiles = V.create $ do
  vm <- VM.new n
  forM_ [0 .. n - 2] $ \i -> do
    let l1 = tiles V.! i
        l2 = tiles V.! (i + 1)
    VM.write vm i $ normEdge (l1, l2)
  let l1 = tiles V.! (n - 1)
      l2 = tiles V.! 0
  VM.write vm (n - 1) $ normEdge (l1, l2)
  pure vm
 where
  n = V.length tiles

-- Assumes all edges are horizontal or vertical, and normalised.
partitionEdges :: V.Vector Edge -> (V.Vector DEdge, V.Vector DEdge)
partitionEdges es =
  let (hEdges, vEdges) = V.partition isHEdge es
  in  (V.map hEdgeToDEdge hEdges, V.map vEdgeToDEdge vEdges)
 where
  isHEdge ((_, yi), (_, yj)) = yi == yj
  hEdgeToDEdge ((xi, yi), (xj, _)) = (yi, xi, xj)
  vEdgeToDEdge ((xi, yi), (_, yj)) = (xi, yi, yj)

isFull :: V.Vector DEdge -> V.Vector DEdge -> Rect -> Bool
isFull hDEdges vDEdges rect' =
  let rect = normRect rect'
  in  isCornersOK && isEdgesOK rect && (isInsideOK rect)
 where
  isCornersOK =
    let corners = otherCorners rect'
    in  L.all cornerOK corners
   where
    otherCorners :: Rect -> [Loc]
    otherCorners ((xi, yi), (xj, yj)) = [(xj, yi), (xi, yj)]

    cornerOK tile = onEdge tile || inArea tile

    onEdge :: Loc -> Bool
    onEdge (x, y) = onHDEdge || onVDEdge
     where
      p (c1, c2) (c, ci, cj) = c1 == c && c2 >= ci && c2 <= cj
      onHDEdge = V.any (p (y, x)) hDEdges
      onVDEdge = V.any (p (x, y)) vDEdges

  isEdgesOK ((xi, yi), (xj, yj)) =
    let (rectHEdges, rectVEdges) = rectEdges
        hEdgesOK = L.all hEdgeOK rectHEdges
        vEdgesOK = L.all vEdgeOK rectVEdges
    in  hEdgesOK && vEdgesOK
   where
    -- Assumes the rectangle is normalised.
    rectEdges :: ([DEdge], [DEdge])
    rectEdges =
      ([(yi, xi, xj), (yj, xi, xj)], [(xi, yi, yj), (xj, yi, yj)])
    hEdgeOK hDEdge = not $ V.any (edgeCross hDEdge) vDEdges
    vEdgeOK vDEdge = not $ V.any (edgeCross vDEdge) hDEdges

  isInsideOK ((xi, yi), _) = inArea inRect
   where
    -- Assumes that the rectangle is normalised
    inRect :: Loc
    inRect = (xi + 1, yi + 1)

  inArea :: Loc -> Bool
  inArea (x, y) =
    let vDEdges' = V.map vEdgeShift vDEdges
    in  odd $ V.foldl' countV 0 vDEdges'
   where
    countV :: Int -> DEdge -> Int
    countV acc vDEdge = if edgeCross (y, 0, x) vDEdge then acc + 1 else acc

    vEdgeShift :: DEdge -> DEdge
    vEdgeShift (x', y1, y2) =
      let y1' = if y == y1 then y1 + 1 else y1
          y2' = if y == y2 then y2 + 1 else y2
      in  (x', y1', y2')
