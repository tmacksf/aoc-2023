module Main where

readInputPt1 :: FilePath -> IO [[String]]
readInputPt1 fname = (map parseLine . lines) <$> readFile fname
    where parseLine xs = words xs 

data Race = Race 
  { time :: Int 
  , distance :: Int 
  } deriving (Show)

type Races = [Race]

readRaces :: [[String]] -> Races 
readRaces [] = []
readRaces (x:y:[]) = go (tail x) (tail y)
  where
    go :: [String] -> [String] -> Races
    go [] _ = []
    go _ [] = []
    go (i:is) (j:js) = Race (read i) (read j) : go is js 
readRaces_ = error "Error"

wins :: Int -> Race -> Int
wins holding r
  | holding < (time r) = (if dist > (distance r) then 1 else 0) + wins (holding + 1) r
  | otherwise = 0
    where dist = ((time r) - holding) * holding

part1 :: Races -> Int
part1 [] = 1
part1 (x:xs) = (wins 0 x) * (part1 xs)

-- Part 2
readInputPt2 :: FilePath -> IO [[String]]
readInputPt2 fname = (map parseLine . lines) <$> readFile fname
    where parseLine xs = words xs 

readRaces2 :: [[String]] -> Race 
readRaces2 [] = Race 0 0 
readRaces2 (x:y:[]) = tToRace (go2 (tail x) (tail y))
  where
    go2 :: [String] -> [String] -> (String, String)
    go2 [] _ = ("", "") 
    go2 _ [] = ("", "") 
    go2 (i:is) (j:js) = (i ++ fst n, j ++ snd n)
      where n = go2 is js
readRaces2 _ = error "Error"

tToRace :: (String, String) -> Race
tToRace (x, y) = Race (read x) (read y)

main = do
  x <- readInputPt1 "input.txt"
  print $ wins 0 (readRaces2 x)
