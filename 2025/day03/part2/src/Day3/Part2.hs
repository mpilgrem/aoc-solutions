module Day3.Part2
  ( solution
  ) where

solution :: String -> Int
solution = sum . map (value . best 12 . readBank) . lines

readBank :: String -> [Int]
readBank = map (read . pure)

best :: Int -> [Int] -> [Int]
best n = go (n - 1) []
 where
  go 0 ys xs =
    let (m, _) = best' 0 xs
    in  (m : ys)
  go n' ys xs =
    let (m, xs') = best' n' xs
    in  go (n' - 1) (m : ys) xs'

best' :: Int -> [Int] -> (Int, [Int])
best' n xs =
  let l = length xs
      stubN = l - n
      (stub, rest) = splitAt stubN xs
      m = maximum stub
      (_, rest') = break (== m) stub
  in  case rest' of
        [] -> error "The impossible happened!"
        (_ : rest'') -> (m, rest'' <> rest)

value :: [Int] -> Int
value [] = error "No values"
value [x] = x
value (x : xs) = x + 10 * value xs
