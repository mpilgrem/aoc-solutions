module Day5.Part1
  ( solution
  ) where

import           AdventOfCode ( breakUpWith )

solution :: String -> Int
solution s =
  let (rs, is) = readDb s
      fs = filter (isFresh rs) is
  in  length fs

readDb :: String -> ([(Int, Int)], [Int])
readDb s =
  let ls = lines s
  in  case breakUpWith "" ls of
        [rs', is'] -> (map readRange rs', map read is')
        _ -> error "Cannot parse as a database"

readRange :: String -> (Int, Int)
readRange s = case breakUpWith '-' s of
  [l, h] -> (read l, read h)
  _ -> error $ "Cannot parse as range: " <> s

inRange :: (Int, Int) -> Int -> Bool
inRange (l, h) i = i >= l && i <= h

isFresh :: [(Int, Int)] -> Int -> Bool
isFresh rs i = any (`inRange` i) rs
