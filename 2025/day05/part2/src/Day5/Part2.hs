module Day5.Part2
  ( solution
  ) where

import           AdventOfCode ( breakUpWith )
import           Data.List ( sort )

data Range = Range !Int !Int
  deriving (Eq, Show)

instance Ord Range where

  compare (Range l1 h1) (Range l2 h2)
    | l1 == l2 = compare h1 h2
    | otherwise = compare l1 l2

solution :: String -> Int
solution s =
  let (rs, _) = readDb s
  in  sum $ map rangeSize $ union rs

union :: [Range] -> [Range]
union [] = []
union [r] = [r]
union (r1 : r2 : rs) = case union' r1 r2 of
  Right r -> union (r : rs)
  Left (r1', r2') -> r1' : union (r2' : rs)

union' :: Range -> Range -> Either (Range, Range) Range
union' r1@(Range l1 h1) r2@(Range l2 h2)
  | l2 > h1 + 1 = Left (r1, r2)
  | otherwise = Right (Range (min l1 l2) (max h1 h2))

rangeSize :: Range -> Int
rangeSize (Range l h) = h - l + 1

readDb :: String -> ([Range], [Int])
readDb s =
  let ls = lines s
  in  case breakUpWith "" ls of
        [rs', is'] -> (sort $ map readRange rs', map read is')
        _ -> error "Cannot parse as a database"

readRange :: String -> Range
readRange s = case breakUpWith '-' s of
  [l, h] -> Range (read l) (read h)
  _ -> error $ "Cannot parse as range: " <> s
