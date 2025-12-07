module Day2.Part2
  ( solution
  ) where

import           AdventOfCode ( breakUpWith )

solution :: String -> Int
solution = sum . map sumInvalid . breakUpWith ','

validId :: String -> Bool
validId s = notElem s $ badIds s

toRange :: String -> (Int, Int)
toRange s = case breakUpWith '-' s of
    [firstPart, secondPart] -> (read firstPart, read secondPart)
    _ -> error $ "Cannot parse as range: " <> s

ids :: (Int, Int) -> [String]
ids (low, high) = map show [low .. high]

sumInvalid :: String -> Int
sumInvalid = sum . map read . filter (not . validId) . ids . toRange

badId :: Int -> String -> String
badId n stub = concat $ replicate n stub

badIds :: String -> [String]
badIds s =
  let l = length s
  in  map (\n -> badId (l `div` n) (take n s)) $ stubs l

stubs :: Int -> [Int]
stubs l = filter (\n -> l `mod` n == 0)  [1 .. l `div` 2]
