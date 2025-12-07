module Day2.Part1
  ( solution
  ) where

solution :: String -> Int
solution = sum . map sumInvalid . breakUp

validId :: String -> Bool
validId s = firstHalf /= secondHalf
 where
  l = length s
  (firstHalf, secondHalf) = splitAt (l `div` 2) s

toRange :: String -> (Int, Int)
toRange s =
  let (firstPart, secondPart') = break (== '-') s
  in  case secondPart' of
        [] -> error "Second part of range is missing"
        (_ : secondPart) -> (read firstPart, read secondPart)

ids :: (Int, Int) -> [String]
ids (low, high) = map show [low .. high]

sumInvalid :: String -> Int
sumInvalid = sum . map read . filter (not . validId) . ids . toRange

breakUp :: String -> [String]
breakUp s =
  let (first, rest) = break (== ',') s
  in  case rest of
        "" -> [first]
        "," -> [first]
        (',' : cs) -> first : breakUp cs
        _ -> error "The impossible happened!"
