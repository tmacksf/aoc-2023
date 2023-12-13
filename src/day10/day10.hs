module Main where

data Direction = North | South | East | West | None deriving (Eq, Show)

readInputPt1 :: FilePath -> IO [String] 
readInputPt1 fname = (map parseLine . lines) <$> readFile fname
    where parseLine xs = xs

findStart :: [String] -> (Int, Int) -> (Int, Int)
findStart ([]:xs) (i, _) = findStart xs (i+1, 0)
findStart ((y:ys):xs) (i, j) = if y == 'S' then (i, j) else findStart (ys:xs) (i, j+1)
findStart [] _ = error "No start"

-- Globals
isize = 5
jsize = 5

-- Board, previous, current
followPath :: [String] -> (Int, Int) -> Direction -> Int
followPath xs (i, j) d 
  -- | i >= isize || i < 0 || j >= jsize || j < 0 = error "err" 
  | (xs !! i) !! j == 'S' = 0 
  | otherwise = 1 + followPath xs (test (i, j) nextd) nextd
    where nextd = nextDir d ((xs !! i) !! j)

test :: (Int, Int) -> Direction -> (Int, Int)
test (x, y) d
  | d == North = (x - 1, y)
  | d == South = (x + 1, y)
  | d == East = (x, y + 1)
  | otherwise = (x, y - 1)

nextDir :: Direction -> Char -> Direction
nextDir _ '.' = error "err"
nextDir North c
  | c == '|' = North
  | c == '7' = West
  | otherwise = East -- F
nextDir South c 
  | c == '|' = South
  | c == 'J' = West
  | otherwise = East -- L
nextDir East c 
  | c == '-' = East
  | c == 'J' = North
  | otherwise = South -- 7
nextDir West c 
  | c == '-' = West
  | c == 'L' = North
  | otherwise = South -- F

solution1 :: [String] -> Int 
solution1 xs = followPath xs (63, 79) North

-- Part 2
createPath :: [String] -> (Int, Int) -> Direction -> [String]
createPath xs (i, j) d 
  | (xs !! i) !! j == 'S' = xs 
  | otherwise = createPath (replaceAt (i, j) xs) (test (i, j) nextd) nextd
    where nextd = nextDir d ((xs !! i) !! j)

replaceAt :: (Int, Int) -> [String] -> [String]
replaceAt (i, j) xs = fst strs ++ (replaceInd j 'P' (head $ snd strs)) : (tail $ snd strs)
  where strs = splitAt i xs

replaceInd :: Int -> Char -> String -> String
replaceInd j _ [] = []
replaceInd j c xs = fst strs ++ (c : (tail $ snd strs))
  where strs = splitAt j xs 

removeEverything :: [String] -> [String]
removeEverything xs = map go xs
  where 
    go :: String -> String
    go ys = [if i == '.' || i == 'P' || i == 'S' then i else ' ' | i <- ys]
    --go ys = [i  | i <- ys, (i == '.' || i == 'P' || i == 'S')]

main = do
  x <- readInputPt1 "input.txt"
  print $ findStart x (0, 0)
  print $ (removeEverything (createPath x (63, 79) North))

