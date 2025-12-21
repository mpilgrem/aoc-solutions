module Day10.Part2 where

import           AdventOfCode ( breakUpWith )
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as V
import qualified Day10.Part2.ILP as LP

type Button = S.Set Int
type Output = [Int]
type Equation = (Int, S.Set Int)

solution :: String -> Int
solution = L.sum . L.map (solve . equations) . readMachs

switches :: (Output, [Button]) -> (Int, [S.Set Int])
switches (o, bs) = (L.length bs, map buttonsWithSwitch [0 .. n - 1])
 where
  n = L.length o
  indexButtons = L.zip bs [0   ..]
  buttonsWithSwitch i =
    S.fromList $ map snd $ filter (hasSwitch i . fst) indexButtons

hasSwitch :: Int -> Button -> Bool
hasSwitch i b = i `S.member` b

equations :: (Output, [Button]) -> (Int, [Equation])
equations (o, bs) =
  let (bn, sss) = switches (o, bs)
  in  (bn, L.zip o sss)

solve :: (Int, [Equation]) -> Int
solve (n, es) =
  let m = length es
      (a, b) = LP.toAb m n es
  in  maybe (error "No solution!") V.sum $ LP.solve m n a b

readMachs :: String -> [(Output, [Button])]
readMachs = map readMach . lines

readMach :: String -> (Output, [Button])
readMach s =
  let ss = breakUpWith ' ' s
      err = error "Cannot parse machine!"
  in  case ss of
        [] -> err
        (_ : rest) -> case L.unsnoc rest of
          Nothing -> err
          Just (bs, joltage) -> case bs of
            [] -> err
            _  -> (readJoltage joltage, readButtons bs)

readJoltage :: String -> Output
readJoltage [] = errorJoltage
readJoltage ('{' : rest) = case L.unsnoc rest of
  Nothing -> errorJoltage
  Just (joltages , '}') -> case breakUpWith ',' joltages of
    [] -> errorJoltage
    js -> map read js
  _ -> errorJoltage
readJoltage _ = errorJoltage

errorJoltage :: a
errorJoltage = error "Cannot parse joltage!"

readButtons :: [String] -> [Button]
readButtons = map readButton

readButton :: String -> Button
readButton [] = errorButton
readButton ('(' : rest) = case L.unsnoc rest of
  Nothing -> errorButton
  Just (buttons , ')') -> case breakUpWith ',' buttons of
    [] -> errorButton
    bs -> S.fromList $ map read bs
  _ -> errorButton
readButton _ = errorButton

errorButton :: a
errorButton = error "Cannot parse button!"
