module Day10.Part2 where

import           AdventOfCode ( breakUpWith )
import           Data.Either ( partitionEithers )
import qualified Data.List as L
import qualified Data.Set as S

type Button = S.Set Int
type Output = [Int]
type Equation = (Int, S.Set Int)

solution :: String -> Int
solution
  = sum
  . map (maximum . map fst . merge2 . snd . simplifiedEquations)
  . readMachs

switches :: (Output, [Button]) -> (Int, [S.Set Int])
switches (o, bs) = (L.length bs, map buttonsWithSwitch [0 .. n - 1])
 where
  n = L.length o
  indexButtons = L.zip bs [0 ..]
  buttonsWithSwitch i =
    S.fromList $ map snd $ filter (hasSwitch i . fst) indexButtons

hasSwitch :: Int -> Button -> Bool
hasSwitch i b = i `S.member` b

equations :: (Output, [Button]) -> (Int, [Equation])
equations (o, bs) =
  let (n, ss) = switches (o, bs)
  in  (n, L.zip o ss)

sortEquations :: [Equation] -> [Equation]
sortEquations = L.sortOn (S.size . snd)

sortedEquations :: (Output, [Button]) -> (Int, [Equation])
sortedEquations (o, bs) =
  let (n, es) = equations (o, bs)
  in  (n, sortEquations es)

simplifiedEquations :: (Output, [Button]) -> (Int, [Equation])
simplifiedEquations (o, bs) =
  let (n, es) = sortedEquations (o, bs)
  in  (n, simplify es)

simplify :: [Equation] -> [Equation]
simplify = simplify' []

simplify' :: [Equation] -> [Equation] -> [Equation]
simplify' done [] = sortEquations done
simplify' done (e@(o, ss) : rest) =
  let (done', doneChanged) = deduct done
      (rest', restChanged) = deduct rest
  in  simplify' (e : done') (doneChanged <> restChanged <> rest')
 where
  deduct :: [Equation] -> ([Equation], [Equation])
  deduct [] = ([], [])
  deduct (e2@(o2, ss2) : es)
    | ss `S.isSubsetOf` ss2 =
        let ss2' = ss2 `S.difference` ss
        in  if S.null ss2'
              then (unchanged, changed)
              else (unchanged, (o2 - o, ss2') : changed)
    | otherwise = (e2 : unchanged, changed)
   where
    (unchanged, changed) = deduct es

merge2 :: [Equation] -> [Equation]
merge2 [] = []
merge2 [e] = [e]
merge2 (e : es) = case merge1 e es of
  Left result -> e : merge2 result
  Right result -> merge2 result

merge1 :: Equation -> [Equation] -> Either [Equation] [Equation]
merge1 e es =
  let (ls, re) = partitionEithers $ map (merge e) es
      result = ls <> re
  in  case re of
        [] -> Left result
        _  -> Right result

merge :: Equation -> Equation -> Either Equation Equation
merge (o1, ss1) e2@(o2, ss2) = if S.disjoint ss1 ss2
  then Right (o1 + o2, S.union ss1 ss2)
  else Left e2

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
readButtons ss = map readButton ss

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
