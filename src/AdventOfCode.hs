{-# LANGUAGE LambdaCase #-}

module AdventOfCode
  ( defaultMain
  , defaultTestMain
  , breakUp
  , breakUpWith
  ) where

import           System.Environment ( getArgs, withArgs )

defaultMain :: Show a => (String -> a) -> IO ()
defaultMain solution = getArgs >>= \case
  [] -> error "No file specified."
  (fp : _) -> readFile fp >>= print . solution

defaultTestMain :: Show a => (String -> a) -> IO ()
defaultTestMain = (withArgs ["test/data.txt"]) . defaultMain

breakUp :: (a -> Bool) -> [a] -> [[a]]
breakUp _ [] = []
breakUp p xs =
  let (x, rest') = break p xs
  in  case rest' of
        [] -> [x]
        [_] -> [x]
        (_ : rest) -> x : breakUp p rest

breakUpWith :: Eq a => a -> [a] -> [[a]]
breakUpWith x = breakUp (== x)
