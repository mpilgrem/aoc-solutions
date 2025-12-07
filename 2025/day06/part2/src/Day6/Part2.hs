module Day6.Part2
  ( solution
  ) where

import           AdventOfCode ( breakUp )
import           Data.List ( transpose, unsnoc )
import           Data.Maybe ( fromMaybe )

data Operator
  = Add
  | Mult
  deriving (Eq, Show)

solution :: String -> Int
solution = sum . uncurry results . readLines

readLines :: String -> ([[Int]], [Operator])
readLines s =
  let ls = lines s
      (nss, os) = fromMaybe (error "Input is malformed") (unsnoc ls)
  in  (toNums $ transpose nss, readOps os)

toNums :: [String] -> [[Int]]
toNums [] = [[]]
toNums nss = map (map read) $ breakUp isBlank nss
 where
  isBlank = all (== ' ')

readOps :: String -> [Operator]
readOps = map readOp . words

readOp :: String -> Operator
readOp "+" = Add
readOp "*" = Mult
readOp o = error $ "Unexpected operator: " <> o

toIdentity :: Operator -> Int
toIdentity Add = 0
toIdentity Mult = 1

op :: Operator -> [Int] -> Int
op o = foldl' (op' o) (toIdentity o)

op' :: Operator -> Int -> Int -> Int
op' Add x y = x + y
op' Mult x y = x * y

results :: [[Int]] -> [Operator] -> [Int]
results nss os = zipWith op os nss
