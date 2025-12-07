module Day2.Part2
  ( solution
  ) where

solution :: String -> Int
solution = sum . map sumInvalid . breakUp

validId :: String -> Bool
validId s = notElem s $ badIds s

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

badId :: Int -> String -> String
badId n stub = concat $ replicate n stub

badIds :: String -> [String]
badIds s =
  let l = length s
  in  map (\n -> badId (l `div` n) (take n s)) $ stubs l

stubs :: Int -> [Int]
stubs l = filter (\n -> l `mod` n == 0)  [1 .. l `div` 2]
