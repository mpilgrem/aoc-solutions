module Day5.Part1
  ( solution
  ) where

solution :: String -> Int
solution s =
  let (rs, is) = readDb s
      fs = filter (isFresh rs) is
  in  length fs

readDb :: String -> ([(Int, Int)], [Int])
readDb s =
  let ls = lines s
      (rs', is'') = break (== "") ls
      rs = map readRange rs'
      is = case is'' of
             [] -> error "Second part of the database is missing"
             (_: is') -> map read is'
  in  (rs, is)

readRange :: String -> (Int, Int)
readRange s =
  let (l, h') = break (== '-') s
      h = case h' of
            [] -> error "Second part of range is missing!"
            (_ : h'') -> h''
  in  (read l, read h)

inRange :: (Int, Int) -> Int -> Bool
inRange (l, h) i = i >= l && i <= h

isFresh :: [(Int, Int)] -> Int -> Bool
isFresh rs i = any (`inRange` i) rs
