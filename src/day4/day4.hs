module Main where

data Card = Card
  { cardId :: Int
  , nums :: [Int]
  , winning :: [Int]
  } deriving (Show)

parseCard :: [String] -> Card 
parseCard [] = Card 0 [] []
parseCard (x:xs) = 
    Card (read . takeWhile (/= ':') $ head xs)
    (map read $ fst nums) (map read $ tail $ snd nums)
    where nums = span (/="|") $ tail xs

readInputPt1 :: FilePath -> IO [Card]
readInputPt1 fname = (map parseLine . lines) <$> readFile fname
    where parseLine xs = parseCard $ words xs 

gameScore :: Card -> Int
gameScore g = if go (nums g) (winning g) > 0 then 2 ^ ((go (nums g) (winning g)) - 1) else 0
  where 
    go :: [Int] -> [Int] -> Int
    go [] _ = 0
    go (x:xs) (ys) = if x `elem` ys then 1 + go xs ys else go xs ys

readInputPt2 :: FilePath -> IO [CardCount]
readInputPt2 fname = (map parseLine . lines) <$> readFile fname
    where parseLine xs = CardCount (parseCard $ words xs) 1

data CardCount = CardCount 
  { c :: Card
  , count :: Int
  } deriving (Show)

winningCount :: Card -> Int
winningCount c = go (nums c) (winning c)
  where
    go :: [Int] -> [Int] -> Int
    go [] _ = 0
    go (x:xs) (ys) = if x `elem` ys then 1 + go xs ys else go xs ys

playCards :: [CardCount] -> [CardCount]
playCards [] = []
playCards (x:xs) = x : playCards (duplicateCards (count x) score xs) 
  where score = winningCount (c x)

duplicateCards :: Int -> Int -> [CardCount] -> [CardCount]
duplicateCards _ _ [] = []
duplicateCards _ 0 xs = xs -- quit early lazily
duplicateCards times depth (x:xs) = 
    (CardCount (c x) ((count x) + times)) : duplicateCards times (depth - 1) xs

main = do
  x <- readInputPt2 "input.txt"
  print $ foldr (\z y -> (count z) + y) 0 (playCards x)
