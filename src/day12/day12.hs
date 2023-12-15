module Main where 

import Data.Char (isDigit, digitToInt)

readInputPt1 :: FilePath -> IO [([Char], [Int])]
readInputPt1 fname = (map parseLine . lines) <$> readFile fname
    where parseLine xs = decodeIn xs

decodeIn :: String -> ([Char], [Int]) 
decodeIn xs = ((head w), (map (read) (readGroupings $ last w)))
  where w = words xs 

readGroupings :: [Char] -> [String]
readGroupings [] = []
readGroupings xs 
  | (head xs) == ',' = readGroupings $ tail xs
  | otherwise = [(takeWhile (/= ',') xs)] ++ (readGroupings $ dropWhile (/= ',') xs)


parseRecord :: [Char] -> [Int] -> Int
parseRecord [] [] = 1
parseRecord [] _ = 0
parseRecord xs [] = if elem '#' xs then 0 else 1
parseRecord xs ys
  | head xs == '.' = parseRecord (tail xs) ys -- continue
  | head xs == '#' = if validBlock xs ys then parseRecord (drop ((head ys) + 1) xs) (tail ys) else 0
  | otherwise = (parseRecord (tail xs) ys) + if validBlock xs ys
    then parseRecord (drop ((head ys) + 1) xs) (tail ys) else 0

validBlock :: [Char] -> [Int] -> Bool
validBlock (x:xs) (y:ys)
  | y > (length (x:xs)) = False
  | elem '.' (take y (x:xs)) = False
  | (y == length (x:xs) || (x:xs) !! y /= '#') = True
  | otherwise = False 
validBlock _ _ = error "error" 

part1 :: [([Char], [Int])] -> Int
part1 xs = sum $ map (\x -> parseRecord (fst x) (snd x)) xs

main = do
  x <- readInputPt1 "input.txt"
  print $ part1 x
