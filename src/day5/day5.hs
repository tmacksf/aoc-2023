module Main where

import Data.Char
import Data.List
import Data.Function (on)

data Almanac = Almanac 
  { seeds :: [Int]
  , seed_soil :: [(Int, Int, Int)]
  , soil_fert :: [(Int, Int, Int)]
  , fert_water :: [(Int, Int, Int)]
  , water_light :: [(Int, Int, Int)]
  , light_temp :: [(Int, Int, Int)]
  , temp_humidity :: [(Int, Int, Int)]
  , humidity_loc :: [(Int, Int, Int)]
  } deriving (Show)

instance Semigroup Almanac where 
    Almanac s ss sf fs wl lt th hl <> 
      Almanac s1 ss1 sf1 fs1 wl1 lt1 th1 hl1 = 
        Almanac (s <> s1) (ss <> ss1) (sf <> sf1) (fs <> fs1)
          (wl <> wl1) (lt <> lt1) (th <> th1) (hl <> hl1)

readInputPt1 :: FilePath -> IO [[String]]
readInputPt1 fname = (map parseLine . lines) <$> readFile fname
    where parseLine xs = words xs 

-- Big parser
stringToAlmanac :: [[String]] -> Almanac
stringToAlamanc [] = Almanac [] [] [] [] [] [] [] [] 
stringToAlmanac (x:xs) = 
    Almanac (readSeeds x) [] [] [] [] [] [] [] <> (parseAlmanac $ xs)

readSeeds :: [String] -> [Int]
readSeeds [] = []
readSeeds (x:xs) = if isNumber $ head x then (read x) : readSeeds xs else readSeeds xs

parseAlmanac :: [[String]] -> Almanac
parseAlmanac [] = Almanac [] [] [] [] [] [] [] []
parseAlmanac ((y:_):xs)
  | y == "seed-to-soil" = Almanac [] (fst vals) [] [] [] [] [] [] <> parseAlmanac (snd vals)
  | y == "soil-to-fertilizer" = Almanac [] [] (fst vals) [] [] [] [] [] <> parseAlmanac (snd vals)
  | y == "fertilizer-to-water"= Almanac [] [] [] (fst vals) [] [] [] [] <> parseAlmanac (snd vals)
  | y == "water-to-light" = Almanac [] [] [] [] (fst vals) [] [] [] <> parseAlmanac (snd vals)
  | y == "light-to-temperature" = Almanac [] [] [] [] [] (fst vals) [] [] <> parseAlmanac (snd vals)
  | y == "temperature-to-humidity" = Almanac [] [] [] [] [] [] (fst vals) [] <> parseAlmanac (snd vals)
  | y == "humidity-to-location" = Almanac [] [] [] [] [] [] [] (fst vals) <> parseAlmanac (snd vals)
  | otherwise = Almanac [] [] [] [] [] [] [] [] <> parseAlmanac (snd vals)
    where vals = customSpan xs 

customSpan :: [[String]] -> ([(Int, Int, Int)], [[String]])
customSpan [] = ([], []) 
customSpan s = (map stringToTripple (takeWhile (isNumber . head . head) s), 
                dropWhile (isNumber . head . head) s) -- weird way to solve this

stringToTripple :: [String] -> (Int, Int, Int)
stringToTripple [] = (0,0,0)
stringToTripple (x:y:z:[]) = (read x, read y, read z)
-- Parser done

solutionPt1 :: Almanac -> Int
solutionPt1 a = minimum $ foldr (\x y -> (findFinal a x) : y) [] (seeds a)

findFinal :: Almanac -> Int -> Int
findFinal a x = 
    valFind (
      valFind (
        valFind (
          valFind (
            valFind (
              valFind (
                valFind x (seed_soil a)
              ) (soil_fert a)
            ) (fert_water a)
          ) (water_light a)
        ) (light_temp a)
      ) (temp_humidity a)
    ) (humidity_loc a)

-- takes a value and the next mapping and returns the output
valFind :: Int -> [(Int, Int, Int)] -> Int
valFind val [] = val
valFind val ((x, y, z):xs)
  | val >= y && val < (y + z) = (val - y) + x
  | otherwise = valFind val xs


-- Part 2
type Mapping = [(Int, Int, Int)]

data Almanac2 = Almanac2 
  { seedrange :: [(Int, Int)]
  , mappings :: [Mapping]
  } deriving (Show)

instance Semigroup Almanac2 where 
    Almanac2 s mp <> Almanac2 s1 mp1 =  
      Almanac2 (s <> s1) (mp <> mp1) 

-- Big parser
readSeeds2 :: [String] -> [(Int, Int)]
readSeeds2 [] = []
readSeeds2 (x:l:xs) = if isNumber $ head x 
    then (read x, read l) : (readSeeds2 xs)
    else readSeeds2 (l:xs)

stringToAlmanac2 :: [[String]] -> Almanac2
stringToAlamanc2 [] = Almanac [] [] 
stringToAlmanac2 (x:xs) = 
    Almanac2 (readSeeds2 x) [] <> (parseAlmanac2 $ xs)

parseAlmanac2 :: [[String]] -> Almanac2
parseAlmanac2 [] = Almanac2 [] [] 
parseAlmanac2 ((y:_):xs)
  | y == "seed-to-soil" = Almanac2 [] [fst vals] <> parseAlmanac2 (snd vals)
  | y == "soil-to-fertilizer" = Almanac2 [] [fst vals] <> parseAlmanac2 (snd vals)
  | y == "fertilizer-to-water"= Almanac2 [] [fst vals]  <> parseAlmanac2 (snd vals)
  | y == "water-to-light" = Almanac2 [] [fst vals] <> parseAlmanac2 (snd vals)
  | y == "light-to-temperature" = Almanac2 [] [fst vals] <> parseAlmanac2 (snd vals)
  | y == "temperature-to-humidity" = Almanac2 [] [fst vals] <> parseAlmanac2 (snd vals)
  | y == "humidity-to-location" = Almanac2 [] [fst vals] <> parseAlmanac2 (snd vals)
  | otherwise = Almanac2 [] [] <> parseAlmanac2 (snd vals)
    where vals = customSpan xs 

readInputPt2 :: FilePath -> IO [[String]]
readInputPt2 fname = (map parseLine . lines) <$> readFile fname
    where parseLine xs = words xs 

valFind2 :: (Int, Int) -> Mapping -> [(Int, Int)]
valFind2 val [] = [val] 
valFind2 (s, l) ((x, y, z):xs)
  | s >= y && s < (y + z) = if (s + l) <= (y + z) 
    then [(s - y + x, l)] 
    else (s - y + x, (y + z) - s) : valFind2 (y + z, l - (y + z - s)) xs --(val - y) + x
  | otherwise = valFind2 (s, l) xs

solve2 :: Almanac2 -> Int -- Int
solve2 a = minimum $ map (fst) $ recursiveMappings (seedrange as) (mappings as)
  where as = a {mappings = map (sortMappings) (mappings a)}

sortMappings :: Mapping -> Mapping
sortMappings = sortBy (compare `on` (\(a,b,c)-> b))

singleMapRound :: [(Int, Int)] -> Mapping -> [(Int, Int)]
singleMapRound _ [] = []
singleMapRound [] _ = []
singleMapRound (x:xs) y = (valFind2 x y) ++ (singleMapRound xs y)

recursiveMappings :: [(Int, Int)] -> [Mapping] -> [(Int, Int)]
recursiveMappings x [] = x
recursiveMappings [] _ = error "Should not be here"
recursiveMappings vals (x:xs) = recursiveMappings (singleMapRound vals x) xs

main = do
  x <- readInputPt1 "input.txt" 
  print $ solve2 $ stringToAlmanac2 (filter (/= []) x) -- solutionPt1 ((stringToAlmanac2 (filter (/= []) x)))
