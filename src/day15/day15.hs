module Main where

-- Have to run stack repl (not ghci) because split was installed by stack

import Data.List.Split
import Data.Char (ord)

readInputPt1 :: FilePath -> IO Int 
readInputPt1 fname = decode <$> readFile fname
    where decode xs = eval $ splitOn "," (head $ lines xs)

eval :: [String] -> Int 
eval [] = 0 
eval (x:xs) = evalSingle 0 x + (eval xs)
  where 
    evalSingle i [] = i
    evalSingle i (x:xs) = evalSingle ((i + ord x) * 17 `mod` 256) xs

part1 :: [String] -> Int
part1 _ = 0

main = do
  x <- readInputPt1 "input.txt"
  print x 
