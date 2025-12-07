module Day6.Part1
  ( solution
  ) where

import           Data.List ( unsnoc )

data Operator
  = Add
  | Mult
  deriving (Eq, Show)

solution :: String -> Int
solution s =
  let (nss, os) = readLines s
  in  sum $ results os nss

readLines :: String -> ([[Int]], [Operator])
readLines s =
  let ls = lines s
      (nss, os) = case unsnoc ls of
        Just (nss', os') -> (nss', os')
        Nothing -> error "Input is malformed"
  in  (map readNums nss, readOps os)

readNums :: String -> [Int]
readNums s = map read $ words s

readOps :: String -> [Operator]
readOps s = map readOp $ words s

readOp :: String -> Operator
readOp "+" = Add
readOp "*" = Mult
readOp o = error $ "Unexpected operator: " <> o

identity :: [Operator] -> [Int]
identity = map toIdentity
 where
  toIdentity Add = 0
  toIdentity Mult = 1

op :: [Operator] -> [Int] -> [Int] -> [Int]
op = zipWith3 op'

op' :: Operator -> Int -> Int -> Int
op' Add x y = x + y
op' Mult x y = x * y

results :: [Operator] -> [[Int]] -> [Int]
results os = foldl' (op os) (identity os)
