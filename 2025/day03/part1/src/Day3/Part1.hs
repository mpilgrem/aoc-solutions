module Day3.Part1
  ( solution
  ) where

solution :: String -> Int
solution = sum . map (turnOnTwo . readBank) . lines

turnOnTwo :: [Int] -> Int
turnOnTwo [] = error "No batteries in bank"
turnOnTwo [_] = error "One battery in bank"
turnOnTwo (b:bs) = bestTwo (b, bs) bs

bestTwo :: (Int, [Int]) -> [Int] -> Int
bestTwo _ [] = error "No other batteries in bank"
bestTwo (b, bs) [_] = b * 10 + maximum bs
bestTwo (b, bs) (b1: rest) = if b1 > b
  then bestTwo (b1, rest) rest
  else bestTwo (b, bs) rest

readBank :: String -> [Int]
readBank = map (read . pure)
