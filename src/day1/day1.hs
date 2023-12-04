module Main where

import Data.Char (isNumber)

-- Part 1

getNums :: String -> String
getNums ls = [x | x <- ls, isNumber x]

prune :: String -> Int 
prune [] = 0
prune xs = read (head xs : [last xs]) :: Int

readInputPt1 :: FilePath -> IO [Int]
readInputPt1 fname = (map parseLine . lines) <$> readFile fname
    where parseLine xs = prune (getNums xs)

main = do
    x <- readInputPt2 "input1.txt"
    print $ sum x

-- Part 2
-- interesting solution
getNums2 :: String -> String
getNums2 [] = []
getNums2 ('o':'n':'e':xs) = '1' : (getNums2 ('n':'e':xs))
getNums2 ('t':'w':'o':xs) = '2' : (getNums2 ('o':xs))
getNums2 ('t':'h':'r':'e':'e':xs) = '3' : (getNums2 ('e':xs))
getNums2 ('f':'o':'u':'r':xs) = '4' : (getNums2 xs)
getNums2 ('f':'i':'v':'e':xs) = '5' : (getNums2 ('e':xs))
getNums2 ('s':'i':'x':xs) = '6' : (getNums2 xs)
getNums2 ('s':'e':'v':'e':'n':xs) = '7' : (getNums2 ('n':xs))
getNums2 ('e':'i':'g':'h':'t':xs) = '8' : (getNums2 ('t':xs))
getNums2 ('n':'i':'n':'e':xs) = '9' : (getNums2 ('e':xs))
getNums2 (x:xs) = if isNumber x then x : ((getNums2 xs)) else getNums2 xs

readInputPt2 :: FilePath -> IO [Int]
readInputPt2 fname = (map parseLine . lines) <$> readFile fname
    where parseLine xs = prune (getNums2 xs)
