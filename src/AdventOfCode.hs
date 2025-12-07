{-# LANGUAGE LambdaCase #-}

module AdventOfCode
  ( defaultMain
  , defaultTestMain
  ) where

import           System.Environment ( getArgs, withArgs )

defaultMain :: Show a => (String -> a) -> IO ()
defaultMain solution = getArgs >>= \case
  [] -> error "No file specified."
  (fp : _) -> readFile fp >>= print . solution

defaultTestMain :: Show a => (String -> a) -> IO ()
defaultTestMain = (withArgs ["test/data.txt"]) . defaultMain
