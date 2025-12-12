module Day10.Part1
  ( solution
  ) where

import           AdventOfCode ( breakUpWith )
import           Control.Monad ( forM_ )
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import           Data.Semigroup ( Semigroup (..) )
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

newtype Button = Button { unbutton :: V.Vector Bool }
  deriving (Eq, Show)

instance Semigroup Button where

  Button v1 <> Button v2 = Button $ V.zipWith eor v1 v2

type Output = Button

solution :: String -> Int
solution = sum . map solve . readMachs

eor :: Bool -> Bool -> Bool
eor a b = (a || b) && (not a || not b)

solve :: (Output, [Button]) -> Int
solve (output, buttons) =
  let size = L.length buttons
      poss = case L.subsequences [0 .. size -1] of
        [] -> error "The impossible happened!"
        (_ : poss') -> poss'
  in  case L.sortOn fst $ filter isOutput $ map (mach buttons) poss of
        [] -> error "No solution!"
        ((n, _) : _) -> n
 where
  isOutput :: (Int, Button) -> Bool
  isOutput = (== output) . snd

mach :: [Button] -> [Int] -> (Int, Button)
mach _ [] = error "No button presses!"
mach buttons (p : ps) =
  let presses = p NE.:| ps
      size = NE.length presses
  in  (size, sconcat $ NE.map (buttons L.!!) presses)

readMachs :: String -> [(Output, [Button])]
readMachs = map readMach . lines

readMach :: String -> (Output, [Button])
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
              let (o', size) = readOutput o
              in (o', map (readButton size) bs)

readOutput :: String -> (Output, Int)
readOutput [] = errorOutput
readOutput ('[' : rest) = case L.unsnoc rest of
  Nothing -> errorOutput
  Just (output , ']') ->
    let size = L.length output
    in  if size == 0
          then errorOutput
          else (Button $ V.fromList $ map toButton output, size)
  _ -> errorOutput
 where
  toButton '.' = False
  toButton '#' = True
  toButton _ = errorOutput
readOutput _ = errorOutput

errorOutput :: a
errorOutput = error "Cannot parse output!"

readButton :: Int -> String -> Button
readButton _ [] = errorButton
readButton size ('(' : rest) = case L.unsnoc rest of
  Nothing -> errorButton
  Just (buttons , ')') -> case breakUpWith ',' buttons of
    [] -> errorButton
    bs -> Button $ V.create $ do
      vm <- VM.replicate size False
      forM_ bs $ \button ->
        VM.write vm (read button) True
      pure vm
  _ -> errorButton
readButton _ _ = errorButton

errorButton :: a
errorButton = error "Cannot parse button!"
