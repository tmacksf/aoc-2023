module Main where

readInputPt1 :: FilePath -> IO [String]
readInputPt1 fname = lines <$> readFile fname

moveRocks :: [String] -> [String]
moveRocks xs = if si == xs then si else moveRocks si
  where si = singleIter xs

singleIter :: [String] -> [String]
singleIter [] = []
singleIter [x] = [x]
singleIter (x:y:xs) = fst sr : singleIter ((snd sr) : xs)
  where sr = singleMove x y 

singleMove :: String -> String -> (String, String)
singleMove s1 s2 = (
  (map (\(x, y) -> if canMove x y then 'O' else x) $ zip s1 s2), 
  (map (\(x, y) -> if canMove x y then '.' else y) $ zip s1 s2))

canMove :: Char -> Char -> Bool
canMove top bot
  | top == '.' && bot == 'O' = True
  | otherwise = False

part1 :: [String] -> Int
part1 = calcTotal . moveRocks

calcTotal :: [String] -> Int
calcTotal [] = 0
calcTotal (x:xs) = length (x:xs) * (sum [1 | y <- x, y == 'O']) + (calcTotal xs)

main = do
  x <- readInputPt1 "input.txt"
  print $ part1 x
