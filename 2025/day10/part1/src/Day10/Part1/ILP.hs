{-|
The following integer linear programming problem:

    For:

    x_i ∈ {0, 1}, x_i >= 0, i ∈ {0, .., n - 1}

    b_j ∈ {0, 1}, b_j >= 0, j ∈ {0, .., m - 1}

    A_ji ∈ {0, 1}

    Minimise the sum of all x_i subject to Ax = b.

can be solved by brute force when n is small.

Credit: https://www.reddit.com/r/adventofcode/comments/1pk87hl/2025_day_10_part_2_bifurcate_your_way_to_victory/

See: https://pilgrem.com/2025/12/30/integer-linear-programming/
-}

module Day10.Part1.ILP
  ( solve
  , toA
  ) where

import           Data.Bits ( Bits (..) )
import qualified Data.List as L
import           Data.Ord ( comparing )
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as V
import           Data.Word ( Word16 )

type Coefficients = S.Set Int
type VBit = Word16
type Matrix = V.Vector VBit

-- | Helper function to parameterise the solver.
toA :: Int -> Int -> [Coefficients] -> Matrix
toA m n cs = V.generate n $ \i -> foldl' (set i) 0 [0 .. m - 1]
 where
  set i acc j = if i `S.member` (cs !! j) then setBit acc j else acc

solve ::
     Int
     -- ^ n
  -> Matrix
     -- ^ A (m x n)
  -> VBit
     -- ^ b (m x 1)
  -> Maybe VBit
     -- ^ x (n x 1), if a solution exists.
solve n a bMod2 = case filter ((== bMod2) . aTimesMod2) x0s of
  [] -> Nothing
  xs -> Just $ L.minimumBy (comparing popCount) xs
 where
  is = [0 .. n - 1] :: [Int]

  x0s = [0 .. (1 `shiftL` n) - 1] :: [VBit]

  aTimesMod2 x = foldl' xor (0 :: VBit) [ a V.! i | i <- is, testBit x i ]
