module Day7 where

import Data.List.Split (splitOn)

totalCost :: [Int] -> Int -> Int
totalCost ns n = sum $ fmap (\x -> abs (n - x)) ns

-- Part 2
totalCost' :: [Int] -> Int -> Int
totalCost' ns n = sum $ fmap (\x -> cost x n) ns

cost :: Int -> Int -> Int
cost x y = sum [1 .. abs (x - y)]

solve = do
  ns <- fmap read . splitOn "," <$> getContents :: IO [Int]
  let min = minimum ns
  let max = maximum ns
  print $ minimum $ fmap (totalCost' ns) [min .. max]