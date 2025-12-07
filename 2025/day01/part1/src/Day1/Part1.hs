module Day1.Part1
  ( solution
  ) where

solution :: String -> Int
solution = password 50 . readMoves

count ::
     Int
     -- ^ Count so far
  -> Int
     -- ^ Pointer
  -> Int
     -- ^ Move
  -> (Int, Int)
count password' pointer move =
  let pointer' = (pointer + move) `mod` 100
      password'' = if pointer' == 0
        then password' + 1
        else password'
  in (password'', pointer')

password ::
     Int
     -- ^ Initial pointer
  -> [Int]
     -- ^ List of moves
  -> Int
     -- ^ Password
password pointer moves =
  let i = (0, pointer)
  in  fst $ foldl' (uncurry count) i moves

readMoves :: String -> [Int]
readMoves s = map readMove $ lines s

readMove :: String -> Int
readMove [] = error "No move"
readMove [c] = error $ "Invalid move: " <> [c]
readMove (i: cs) = case i of
  'L' -> - readValue cs
  'R' -> readValue cs
  _ -> error $ "Invalid code: " <> [i]

readValue :: String -> Int
readValue s =
  let x = read s
  in  if x > 0
        then x
        else error $ "Invalid distance: " <> show x
