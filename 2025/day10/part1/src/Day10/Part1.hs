module Day10.Part1
  ( solution
  ) where

import           AdventOfCode ( breakUpWith )
import           Data.Bits ( Bits (..) )
import qualified Data.List as L
import qualified Data.Set as S
import           Data.Word ( Word16 )
import qualified Day10.Part1.ILP as LP

type VBit = Word16
type Coefficients = S.Set Int
type Button = S.Set Int
type Output = VBit

solution :: String -> Int
solution = sum . map (solve . equations) . readMachs

solve :: (Int, Output, [Coefficients]) -> Int
solve (n, b, cs) =
  let m = length cs
      a = LP.toA m n cs
  in  maybe (error "No solution!") popCount $ LP.solve n a b

switches :: (Int, [Button]) -> (Int, [S.Set Int])
switches (n, bs) = (L.length bs, map buttonsWithSwitch [0 .. n - 1])
 where
  indexButtons = L.zip bs [0 ..]
  buttonsWithSwitch i =
    S.fromList $ map snd $ filter (hasSwitch i . fst) indexButtons

hasSwitch :: Int -> Button -> Bool
hasSwitch i b = i `S.member` b

equations :: (Int, Output, [Button]) -> (Int, Output, [Coefficients])
equations (n, o, bs) =
  let (bn, sss) = switches (n, bs)
  in  (bn, o, sss)

readMachs :: String -> [(Int, Output, [Button])]
readMachs = map readMach . lines

readMach :: String -> (Int, Output, [Button])
readMach s =
  let ss = breakUpWith ' ' s
      err = error "Cannot parse machine!"
  in  case ss of
        [] -> err
        (o : rest) -> case L.unsnoc rest of
          Nothing -> err
          Just (bs, _joltage) -> case bs of
            [] -> err
            _  ->
              let (n, o') = readOutput o
              in (n, o', readButtons bs)

readOutput :: String -> (Int, Output)
readOutput [] = errorOutput
readOutput ('[' : rest) = case L.unsnoc rest of
  Nothing -> errorOutput
  Just (output , ']') ->
    let n = L.length output
    in  if n == 0
          then errorOutput
          else (n, foldl' set (0 :: VBit) (zip [0 ..] output))
  _ -> errorOutput
 where
  set acc (_, '.') = acc
  set acc (i, '#') = setBit acc i
  set _ _ = errorOutput
readOutput _ = errorOutput

errorOutput :: a
errorOutput = error "Cannot parse output!"

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
