module Main where

readInputPt2 :: FilePath -> IO [[Int]] 
readInputPt2 fname = (map parseLine . lines) <$> readFile fname
    where parseLine xs = map read $ words xs

extrapolate :: [Int] -> [Int] -> [Int]
extrapolate (x:y:xs) ls = extrapolate (y:xs) (ls ++ [y - x])
extrapolate [x] ls = ls ++ [(last (if check ls then [0] else extrapolate ls [])) - x]
extrapolate [] ls = error "bruh"

check :: [Int] -> Bool
check = foldr (\x acc -> (x == 0) && acc) True 

solve1 :: [[Int]] -> Int
solve1 i = sum $ map (last) $ map (\x -> extrapolate x []) i

solve2 :: [[Int]] -> Int
solve2 = solve1 . map reverse

main = do
  i <- readInputPt2 "input.txt"
  print $ solve2 i
