module Main where

import Data.Char 

-- Part 1
data Game = Game 
    { gameId :: Int
    , red :: Int
    , green :: Int
    , blue :: Int
    } deriving (Show, Read, Eq)

instance Semigroup Game where
    Game id x y z <> Game d2 x2 y2 z2 = Game id 
        (if x > x2 then x else x2) 
        (if y > y2 then y else y2) 
        (if z > z2 then z else z2)

readInputPt1 :: FilePath -> IO [Game] 
readInputPt1 fname = (map parseLine . lines) <$> readFile fname
    where parseLine xs = getGame $ removeSpecial xs 

removeSpecial :: String -> String
removeSpecial s = [x | x <- s, (isNumber x) || (isLetter x) || (x == ' ')]

buildGame :: [String] -> Game
buildGame [] = Game 0 0 0 0
buildGame (x:"red":xs) = Game 0 (read x :: Int) 0 0 <> buildGame xs
buildGame (x:"green":xs) = Game 0 0 (read x :: Int) 0 <> buildGame xs
buildGame (x:"blue":xs) = Game 0 0 0 (read x :: Int) <> buildGame xs
buildGame _ = Game 0 0 0 0


getGame :: String -> Game 
getGame xs = (Game (read $ head (drop 1 strList)) 0 0 0) <> (buildGame $ drop 2 strList)
    where strList = words $ removeSpecial xs

filterGames :: [Game] -> Int
filterGames [] = 0
filterGames (x:xs) = if (gameOk x) then (gameId x) + filterGames xs else 0 + filterGames xs

gameOk :: Game -> Bool
gameOk x 
  | red x > 12 = False
  | green x > 13 = False
  | blue x > 14 = False
  | otherwise = True

main = do
    x <- readInputPt1 "input.txt"
    print $ sumGames x

-- Part 2

sumGames :: [Game] -> Int
sumGames [] = 0
sumGames (x:xs) = (red x) * (blue x) * (green x) + (sumGames xs)

