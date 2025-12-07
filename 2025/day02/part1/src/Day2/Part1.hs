module Day2.Part1
  ( solution
  ) where

import           AdventOfCode ( breakUpWith )

solution :: String -> Int
solution = sum . map sumInvalid . breakUpWith ','

validId :: String -> Bool
validId s = firstHalf /= secondHalf
 where
  l = length s
  (firstHalf, secondHalf) = splitAt (l `div` 2) s

toRange :: String -> (Int, Int)
toRange s = case breakUpWith '-' s of
    [firstPart, secondPart] -> (read firstPart, read secondPart)
    _ -> error $ "Cannot parse as range: " <> s

ids :: (Int, Int) -> [String]
ids (low, high) = map show [low .. high]

sumInvalid :: String -> Int
sumInvalid = sum . map read . filter (not . validId) . ids . toRange
