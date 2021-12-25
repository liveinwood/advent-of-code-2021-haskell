module Day7 where

import Data.List.Split (splitOn)

cost :: [Int] -> Int -> Int
cost ns n = sum $ fmap (\x -> abs (n - x)) ns

solve = do
  ns <- fmap read . splitOn "," <$> getContents :: IO [Int]
  let min = minimum ns
  let max = maximum ns
  print $ minimum $ fmap (cost ns) [min .. max]