module Main where

type Mirror = (Vertical, Horizontal)
type Horizontal = [String]
type Vertical = [String]

readInputPt1 :: FilePath -> IO [String]
readInputPt1 fname = lines <$> readFile fname

parseMirror :: [String] -> [Mirror] 
parseMirror [] = []
parseMirror xs 
  | head xs == [] = parseMirror $ drop 1 xs 
  | otherwise = (verticalElements $ fst strs, fst strs) : (parseMirror $ snd strs)
      where strs = span (/= []) xs

verticalElements :: [String] -> [String]
verticalElements [] = []
verticalElements xs 
  | (sum $ map length xs) == 0 = [] -- janky solution
  | otherwise = map head xs : verticalElements (map (drop 1) xs)

-- finds the reflection
findReflection :: [String] -> (Int, Int) -> (Int, Int)
findReflection [] _ = (0, 0)
findReflection xs (i, j) 
  | j >= (length xs) = (0, 0)
  | verifyReflection xs (i, j) = (i, j)
  | otherwise = findReflection xs (j, j+1) 

-- used to check if a reflection exists for a position
verifyReflection :: [String] -> (Int, Int) -> Bool
verifyReflection xs (i, j) 
  | i < 0 || j >= (length xs) = True
  | (xs !! i) == (xs !! j) = True && verifyReflection xs (i-1, j+1)
  | otherwise = False

part1 :: [String] -> Int
part1 xs = sum $ map eachReflection (parseMirror xs)

eachReflection :: Mirror -> Int
eachReflection (v, h)
  | findReflection v (0, 1) == (0, 0) = 100 * (1 + (fst $ findReflection h (0, 1)))-- horizontal
  | otherwise = 1 + (fst $ findReflection v (0, 1)) -- vertical

-- part 2
-- brute force 

generateSubMirrors:: Mirror -> (Int, Int) -> [Mirror]
generateSubMirrors m (i, j)
  | i > (length m) || j > (length $ head m) = []
  | otherwise = 
    where 
      toggleAt :: Mirror -> (Int, Int) -> Mirror
      toggleAt m (x, y) = splitAt 

main = do
    x <- readInputPt1 "input.txt"
    print $ part1 x
    print $ findReflection (fst $ last $ parseMirror x) (0, 1)
