module Main where

readInputPt1 :: FilePath -> IO [String]
readInputPt1 fname = lines <$> readFile fname

moveRocks :: [String] -> [String]
moveRocks xs = singleIter

main = do
  print "test"
