module Main where 


readInputPt1 :: FilePath -> IO [String] 
readInputPt1 fname = (map parseLine . lines) <$> readFile fname
    where parseLine xs = xs

expand :: [String] -> [String]
expand xs = expandH $ expandVert (expandVPoints 0 xs) xs

expandVPoints :: Int -> [String] -> [Int]
expandVPoints i xs
  | i >= length xs = []
  | otherwise = if vertEmpty i xs
    then i : expandVPoints (i+1) xs
    else expandVPoints (i+1) xs

expandVert :: [Int] -> [String] -> [String]
expandVert [] xs = xs 
expandVert (y:ys) xs = expandVert (map (+1) ys) (map (insertEmpty y) xs)

vertEmpty :: Int -> [String] -> Bool
vertEmpty i [] = True
vertEmpty i (x:xs) 
  | x !! i == '#' = False
  | otherwise = True && vertEmpty i xs

insertEmpty :: Int -> String -> String
insertEmpty i xs = fst strs ++ ('.' : snd strs)
  where strs = splitAt i xs

expandH :: [String] -> [String]
expandH [] = []
expandH (x:xs)
  | elem '#' x = x : expandH xs
  | otherwise = x : x : expandH xs

findGalaxies :: [String] -> (Int, Int) -> [(Int, Int)]
findGalaxies [] _ = []
findGalaxies ([]:xs) (i, j) = findGalaxies xs (i+1, 0) 
findGalaxies ((x:xs):ys) (i, j)
  | x == '#' = (i, j) : findGalaxies (xs:ys) (i, j +1)
  | otherwise = findGalaxies (xs:ys) (i, j + 1)

distances :: [(Int, Int)] -> [Int] 
distances [] =[] 
distances (x:xs) = (go x xs) ++ distances xs
  where 
    go :: (Int, Int) -> [(Int, Int)] -> [Int]
    go (i, j) ((x, y):xs) = (abs (i - x) + abs (j - y)) : (go (i, j) xs)
    go _ [] = [] 

solve1 :: [String] -> [Int]
solve1 xs = distances $ findGalaxies (expand xs) (0, 0)

-- Part 2

data Point = Point 
  { galaxy :: Bool
  , hscaleFactor :: Int
  , vscaleFactor :: Int
  } 

instance Show Point where
  show (Point g h v) = show g ++ " H: " ++ show h ++ " V: " ++ show v

-- current scale factor
sf = 1000000 

expand2 :: [String] -> [[Point]]
expand2 xs = expandH2 $ expandVert2 0 (pts xs)
  where 
    pts :: [String] -> [[Point]]
    pts [] = []
    pts (y:ys) = (map (toPt) y) : pts ys

toPt :: Char -> Point
toPt c
  | c == '#' = Point True 1 1
  | otherwise = Point False 1 1

expandVert2 :: Int -> [[Point]] -> [[Point]]
expandVert2 i xs 
  | i >= length xs = xs 
  | otherwise = if vertEmpty2 i xs
    then expandVert2 (i + 1) (map (vMult i) xs)
    else expandVert2 (i + 1) xs

vMult :: Int -> [Point] -> [Point]
vMult i xs = (fst pts) ++ ((Point False 1 sf) : (tail (snd pts)))
  where pts = splitAt i xs

vertEmpty2 :: Int -> [[Point]] -> Bool
vertEmpty2 i [] = True
vertEmpty2 i (x:xs) 
  | galaxy (x !! i) == True = False
  | otherwise = True && vertEmpty2 i xs

expandH2 :: [[Point]] -> [[Point]]
expandH2 [] = []
expandH2 (x:xs)
  | containsGalaxy x = x : (expandH2 xs)
  | otherwise = map (\y -> y {hscaleFactor = sf}) x : expandH2 xs 
  where 
    containsGalaxy xs = foldr (\x acc -> galaxy x || acc) False xs

findGalaxies2 :: [[Point]] -> (Int, Int) -> [(Int, Int)]
findGalaxies2 [] _ = []
findGalaxies2 ([]:xs) (i, j) = findGalaxies2 xs (i+1, 0) 
findGalaxies2 ((x:xs):ys) (i, j)
  | galaxy x == True = (i, j) : findGalaxies2 (xs:ys) (i, j +1)
  | otherwise = findGalaxies2 (xs:ys) (i, j + 1)

totalDistance2 :: [[Point]] -> [(Int,Int)] -> Int 
totalDistance2 ps (x:xs) = sum (map (dist ps x) xs) + totalDistance2 ps xs
totalDistance2 _ [] = 0 

dist :: [[Point]] -> (Int, Int) -> (Int, Int) -> Int
dist ps (i, j) (x, y)
  | i == x && j == y = 0
  | i < x = hscaleFactor ((ps !! i) !! j) + dist ps (i +1, j) (x, y)
  | i > x = error "i > x"
  | j > y = vscaleFactor ((ps !! i) !! j) + dist ps (i, j - 1) (x, y)
  | j < y = vscaleFactor ((ps !! i) !! j) + dist ps (i, j + 1) (x, y)
  | otherwise = error "Otherwise error"

solution2 :: [String] -> Int
solution2 xs = totalDistance2 ex (findGalaxies2 ex (0, 0))
  where ex = expand2 xs

main = do
  x <- readInputPt1 "input.txt"
  print $ solution2 x
