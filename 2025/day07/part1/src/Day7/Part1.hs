module Day7.Part1
  ( solution
  ) where

solution :: String -> Int
solution = countSplitters . processLines [] . lines

processLine1 :: String -> String -> String
processLine1 = zipWith process
 where
  process c1 c2
    | c1 == 'S' && c2 == '.' = '|'
    | c1 == '|' && c2 == '.' = '|'
    | c1 == '|' && c2 == '^' = '~'
    | otherwise = c2

processLine2 :: String -> String -> String
processLine2 xs [] = reverse xs
processLine2 xs [x] = reverse (x: xs)
processLine2 [] (x : xs) = processLine2 [x] xs
processLine2 (x: xs) (y1 : y2 : ys) = case y1 of
  '~' -> processLine2 ('|' : '~' : '|': xs) ys
  _ -> processLine2 (y1 : x : xs) (y2 : ys)

processLines :: [String] -> [String] -> [String]
processLines ls [] = reverse ls
processLines [] (l: ls) = processLines [l] ls
processLines (l1: l1s) (l2 : l2s) =
  let l = processLine1 l1 l2
      l' = processLine2 "" l
  in  processLines (l' : l1 : l1s) l2s

countSplitters :: [String] -> Int
countSplitters ss = foldl' count 0 (concat ss)
 where
  count :: Int -> Char -> Int
  count acc c = if c == '~' then acc + 1 else acc
