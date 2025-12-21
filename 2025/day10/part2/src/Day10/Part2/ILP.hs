{-|
The following integer linear programming problem:

    For:

    x_i ∈ ℤ, x_i >= 0, i ∈ {0, .., n - 1}

    b_j ∈ ℤ, b_j >= 0, j ∈ {0, .., m - 1}

    A_ji ∈ {0, 1}

    Minimise the sum of all x_i subject to Ax = b.

can be solved by brute force when n is small, with a branching recursion over
the binary expansion of the solution, with pruning using parity constraints.

Credit: https://www.reddit.com/r/adventofcode/comments/1pk87hl/2025_day_10_part_2_bifurcate_your_way_to_victory/

See: https://pilgrem.com/2025/12/30/integer-linear-programming/
-}

module Day10.Part2.ILP
  ( solve
  , toAb
  ) where

import           Data.Bits ( Bits (..), FiniteBits (..) )
import qualified Data.List as L
import           Data.Maybe ( catMaybes )
import           Data.Ord ( comparing )
import qualified Data.Set as S
import           Data.Word ( Word16 )
import qualified Data.Vector.Unboxed as V

type Equation = (Int, S.Set Int)
type VInt = V.Vector Int
type VBit = Word16
type Matrix = V.Vector VBit

-- | Helper function to parameterise the solver.
toAb :: Int -> Int -> [Equation] -> (Matrix, VInt)
toAb m n es =
  let a = V.generate n $ \i -> foldl' (set i) 0 [0 .. m - 1]
      b = V.fromList $ map fst es
  in (a, b)
 where
  set i acc j = if i `S.member` snd (es !! j) then setBit acc j else acc

solve ::
     Int
     -- ^ m
  -> Int
     -- ^ n
  -> Matrix
     -- ^ A (m x n)
  -> VInt
     -- ^ b (m x 1)
  -> Maybe VInt
     -- ^ x (n x 1), if a solution exists.
solve m n a b
  | V.all (== 0) b = Just zeroN
  | otherwise =
      let xs = catMaybes
            [ V.zipWith (+) x0vec . V.map (* 2) <$> mx'
            | x0 <- filter ((== bMod2) . aTimesMod2) x0s
            , let x0vec = vBitToVInt x0
                  ax0 = aTimes x0vec
                  diffs = V.zipWith (-) b ax0
            , V.all (\diff -> diff >= 0 && even diff) diffs
            , let b' = V.map (`div` 2) diffs
                  mx' = solve m n a b'
            ]
      in  case xs of
            [] -> Nothing
            _ -> Just $ L.minimumBy (comparing V.sum) xs
 where
  is = [0 .. n - 1] :: [Int]

  x0s = [0 .. (1 `shiftL` n) - 1] :: [VBit]

  aTimesMod2 x = foldl' xor (0 :: VBit) [ a V.! i | i <- is, testBit x i ]

  bMod2 = V.ifoldl' set (0 :: VBit) b

  set acc i bi = if odd bi then setBit acc i else acc

  vBitToVInt vBit = V.generate n (\i -> if testBit vBit i then 1 else 0)

  zero size = V.replicate size (0 :: Int)

  zeroM = zero m

  zeroN = zero n

  js 0 = []
  js ai = let j = countTrailingZeros ai in j : js (clearBit ai j)

  aTimes x = V.accum (+) zeroM [ (j, x V.! i) | i <- is, j <- js (a V.! i) ]
