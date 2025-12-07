module Day1.Part1
  ( solution
  ) where

solution :: String -> Int
solution = password 50 . readMoves . lines

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

count ::
     Int
     -- ^ Count so far
  -> Int
     -- ^ Pointer
  -> Int
     -- ^ Move
  -> (Int, Int)
count acc pointer move =
  let pointer' = (pointer + move) `mod` 100
      acc' = if pointer' == 0
        then acc + 1
        else acc
  in (acc', pointer')

readMoves :: [String] -> [Int]
readMoves = map readMove

readMove :: String -> Int
readMove [] = error "No move"
readMove [c] = error $ "Invalid move: " <> [c]
readMove (i: s) = case i of
  'L' -> - readValue s
  'R' -> readValue s
  _ -> error $ "Invalid code: " <> [i]

readValue :: String -> Int
readValue s =
  let x = read s
  in  if x > 0
        then x
        else error $ "Invalid distance: " <> show x
