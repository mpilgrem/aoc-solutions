module Day7.Part2
  ( solution
  ) where

solution :: String -> Int
solution = countBeams . processLines [] . lines

processFirstLine :: String -> [Int]
processFirstLine = map process
 where
  process 'S' = 1
  process '.' = 0
  process c = error $ "Unexpected first line character: " <> [c]

processLine1 :: [Int] -> String -> [Int]
processLine1 = zipWith process
 where
  process c1 c2
    | c1 > 0 && c2 == '.' = c1
    | c1 > 0 && c2 == '^' = -c1
    | otherwise = 0

processLine2 :: [Int] -> [Int] -> [Int]
processLine2 xs [] = reverse xs
processLine2 xs [x] = reverse (x: xs)
processLine2 [] (x : xs) = processLine2 [x] xs
processLine2 (x: xs) (y1 : y2 : ys)
  | y1 < 0 = processLine2 ((y2 - y1) : y1 : (x - y1) : xs) ys
  | otherwise = processLine2 (y1 : x : xs) (y2 : ys)

processLines :: [[Int]] -> [String] -> [[Int]]
processLines ls [] = reverse ls
processLines [] (l: ls) = processLines [processFirstLine l] ls
processLines (l1 : l1s) (l2 : l2s) =
  let l' = processLine1 l1 l2
      l = processLine2 [] l'
  in  processLines (l : l1 : l1s) l2s

countBeams :: [[Int]] -> Int
countBeams ss = foldl' count 0 (last ss)
 where
  count :: Int -> Int -> Int
  count acc c = if c > 0 then acc + c else acc
