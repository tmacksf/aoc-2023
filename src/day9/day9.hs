module Main where

readInput :: FilePath -> IO [[Int]] 
readInput fname = (map parseLine . lines) <$> readFile fname
    where parseLine xs = map read $ words xs

extrapolate :: [Int] -> [Int] -> [Int]
extrapolate (x:y:xs) ls = extrapolate (y:xs) (ls ++ [y - x])
extrapolate [x] ls = ls ++ [(last (if check ls then [0] else extrapolate ls [])) + x]
extrapolate [] ls = error "bruh"

check :: [Int] -> Bool
check = foldr (\x acc -> (x == 0) && acc) True 

part1 :: [[Int]] -> Int
part1 i = sum $ map (last) $ map (\x -> extrapolate x []) i

-- point free!
part2 = part1 . map reverse

main = do
  i <- readInput "input.txt"
  print $ part2 i
