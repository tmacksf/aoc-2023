module Main where

import Data.List 
import Data.Char 
import Data.Function (on)

data Rank = Five | Four | Full | Three | TwoPair | Two | High deriving (Show, Eq)

instance Ord Rank where
  compare x y = compare (r2n x) (r2n y)
    where
      r2n Five = 7 
      r2n Four = 6 
      r2n Full = 5
      r2n Three = 4
      r2n TwoPair = 3
      r2n Two = 2
      r2n High = 1

data Hand = Hand 
  { hand :: Rank 
  , handValue :: Int
  , original :: [Int]
  } deriving (Show, Eq)

-- First is count second is card type
type FreqMapping = [(Int, Char)]

instance Ord Hand where
  compare x y = if compare (hand x) (hand y) == EQ then go (original x) (original y) else compare (hand x) (hand y)
    where 
      go :: [Int] -> [Int] -> Ordering
      go _ [] = EQ
      go [] _ = EQ
      go (x:xs) (y:ys) = if x == y then go xs ys else compare x y

readInputPt1 :: FilePath -> IO [Hand]
readInputPt1 fname = (map parseLine . lines) <$> readFile fname
    where parseLine xs = toHand $ words xs 

toHand :: [String] -> Hand
toHand xs = Hand (decodeHandType $ cardFrequency $ head xs) (read $ last xs) (readCards $ head xs)

-- for decoding a tie
readCards :: String -> [Int]
readCards [] = []
readCards (x:xs) 
  | isNumber x = (ord x - 48) : readCards xs
  | 'T' == x = 10: readCards xs
  | 'J' == x = 11: readCards xs
  | 'Q' == x = 12: readCards xs
  | 'K' == x = 13: readCards xs
  | 'A' == x = 14: readCards xs

cardFrequency :: String -> FreqMapping
cardFrequency [] = []
cardFrequency xs = sortBy (\(_,a) (_,b) -> compare b a) (groupsToFreq (group $ sort xs))

groupsToFreq :: [String] -> FreqMapping
groupsToFreq [] = []
groupsToFreq (x:xs) = (length x, head x) : groupsToFreq xs

decodeHandType :: FreqMapping -> Rank 
decodeHandType [x] = Five
decodeHandType (x:y:[])
  | fst x == 2 || (fst x == 3 && fst y == 2) = Full
  | otherwise = Four
decodeHandType (x:y:z:[])
  | fst x == 3 || fst y == 3 || fst z == 3 = Three
  | otherwise = TwoPair
decodeHandType (x:y:z:a:[]) = Two
decodeHandType _ = High 

-- Part 2
readInputPt2 :: FilePath -> IO [Hand]
readInputPt2 fname = (map parseLine . lines) <$> readFile fname
    where parseLine xs = toHand2 $ words xs 

toHand2 :: [String] -> Hand
toHand2 xs = Hand (if elem 'J' r then 
    decodeHandType2 $ cardFrequency r else 
    decodeHandType $ cardFrequency r) 
    (read $ last xs) (readCards2 $ head xs)
      where r = head xs

-- for decoding a tie
readCards2 :: String -> [Int]
readCards2 [] = []
readCards2 (x:xs) 
  | isNumber x = (ord x - 48) : readCards2 xs
  | 'T' == x = 10: readCards2 xs
  | 'J' == x = 1: readCards2 xs
  | 'Q' == x = 12: readCards2 xs
  | 'K' == x = 13: readCards2 xs
  | 'A' == x = 14: readCards2 xs

decodeHandType2 :: FreqMapping -> Rank 
decodeHandType2 [x] = Five
decodeHandType2 (x:y:[]) = Five -- first two both contain jack so they will be five
decodeHandType2 xs = decodeHandType $ combineFirst (snd c) (fst c) 
  where c = helper $ sortBy (flip compare `on` fst) xs

combineFirst :: FreqMapping -> Int -> FreqMapping
combineFirst f i = (fst h + i, snd h) : tail f
    where h = head f

helper :: FreqMapping -> (Int, FreqMapping)
helper [] = (0, []) 
helper (x:xs)
   | snd x == 'J' = (fst x, xs)
   | otherwise = (fst z, x : snd z)
       where z = helper xs 

main = do
  x <- readInputPt2 "input.txt"
  print $ sum $ map (\(x, y) -> x * handValue y) (zip [1..] (sort x)) 
