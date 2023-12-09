module Main where

import Data.Char (isLetter, isNumber)
import Data.Map (Map, findWithDefault)
import qualified Data.Map as Map

type Directions = String

readInputPt2 :: FilePath -> IO [String] 
readInputPt2 fname = (map parseLine . lines) <$> readFile fname
    where parseLine xs = xs 

extractCoords :: [String] -> [[String]]
extractCoords x = map words $ decodeInput $ tail x

decodeInput :: [String] -> [String]
decodeInput [] = []
decodeInput (x:xs) = clean x : decodeInput xs

clean :: String -> String
clean [] = []
clean (x:xs) = if isNumber x || isLetter x || x == ' ' then x : clean xs else clean xs

extract :: [String] -> (String, (String, String))
extract (x:y:z:[]) = (x, (y, z))
extract _ = error "Error"

-- current string, record of directions for reset, current direction
findTotal :: String -> String -> String -> Map String (String, String) -> Int
findTotal "ZZZ" _ _ _ = 0
findTotal cstr dir [] mp = findTotal cstr dir dir mp
findTotal cstr dir ('R':dirs) mp = 1 + findTotal (snd $ findWithDefault ("AAA", "AAA") cstr mp) dir dirs mp
findTotal cstr dir (_:dirs) mp = 1 + findTotal (fst $ findWithDefault ("AAA", "AAA") cstr mp) dir dirs mp 

-- part 2 
findNodes :: [String] -> [String]
findNodes xs = filter (\x -> (last x) == 'A') xs  

singleIter :: Char -> [String] -> Map String (String, String) -> [String]
singleIter d xs mp = map (\x -> (if d == 'R' then snd else fst)(findWithDefault (error "bad") x mp)) xs

-- do iteration then check
check :: [String] -> Bool
check = foldr (\x acc -> acc && ((last x) == 'Z')) True 

-- brute force
-- fdirs is full list of directions (L/R), d is current and rdirs is remaining
solve2 :: [String] -> String -> String -> Map String (String, String) -> Int
solve2 xs fdirs [] mp = solve2 xs fdirs fdirs mp
solve2 xs fdirs (d:rdirs) mp = if check newState then 1 else 1 + solve2 newState fdirs rdirs mp
  where newState = singleIter d xs mp

findPathLength :: String -> String -> String -> Map String (String, String) -> Integer
findPathLength node fdirs [] mp = findPathLength node fdirs fdirs mp
findPathLength node fdirs (d:rdirs) mp
  | last node == 'Z' = 0
  | otherwise = 1 + findPathLength newState fdirs rdirs mp
    where newState = (if d == 'R' then snd else fst)(findWithDefault (error "bad") node mp)

main = do
  x <- readInputPt2 "input.txt"
  let dirs = head x 
  let m = map extract $ extractCoords $ x
  let nodes = findNodes $ (map fst m) 
  print $ foldr (\acc x -> lcm acc x) 1 (map (\x -> findPathLength x dirs dirs (Map.fromList m)) nodes)
  
  

  --print $ solve2 nodes dirs dirs (Map.fromList m)
  -- print $ findTotal "AAA" dirs dirs (Map.fromList m)

