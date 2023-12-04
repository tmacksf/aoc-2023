module Main where

import Data.Char

readInputPt1 :: FilePath -> IO [[Char]]
readInputPt1 fname = (map parseLine . lines) <$> readFile fname
    where parseLine xs = xs 

checkAdj :: [(Int, Char)] -> [(Int, Char)] -> [(Int, Char)] -> Int
checkAdj s [] next = 0
checkAdj s prev [] = 0
checkAdj s prev next = 0

isSpecial :: Char -> Bool
isSpecial c
  | isNumber c = False
  | c == '.' = False
  | otherwise = True

process :: [[(Int, Char)]] -> Int
process xs = 0

main = do
    x <- readInputPt1 "example1.txt"
    print $ process $ map (zip [1..]) x 
