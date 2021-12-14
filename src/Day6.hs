module Day6 where

import Data.List.Split (splitOn)

solve = do
  ns <- fmap read . splitOn "," <$> getContents :: IO [Int]
  print $ steps 80 ns

steps :: Int -> [Int] -> Int
steps n ns = length . head . (drop n) $ iterate step ns

step :: [Int] -> [Int]
step = concatMap f
  where
    f 0 = [6, 8]
    f n = [n - 1]