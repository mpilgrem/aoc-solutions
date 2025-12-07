module Day5.Part2
  ( solution
  ) where

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
      (rs', is'') = break (== "") ls
      rs = sort $ map readRange rs'
      is = case is'' of
             [] -> error "Second part of the database is missing!"
             (_ : is') -> map read is'
  in  (rs, is)

readRange :: String -> Range
readRange s =
  let (l, h') = break (== '-') s
      h = case h' of
            [] -> error "Second part of range is missing!"
            (_ : h'') -> h''
  in  Range (read l) (read h)
