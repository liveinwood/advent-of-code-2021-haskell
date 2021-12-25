module Day6 where

import Data.List (group, sort)
import Data.List.Split (splitOn)
import Data.Map (Map, fold, fromList, union, (!))

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

-- Part 2
initMap :: [(Int, Int)] -> Map Int Int
initMap ps = (fromList ps) `union` (fromList [(i, 0) | i <- [0 .. 8]])

step' :: Int -> Map Int Int -> Map Int Int
step' 0 m = m
step' x m = step' (x -1) $ fromList [(0, m ! 1), (1, m ! 2), (2, m ! 3), (3, m ! 4), (4, m ! 5), (5, m ! 6), (6, m ! 7 + m ! 0), (7, m ! 8), (8, m ! 0)]

solve' = do
  ns <- fmap read . splitOn "," <$> getContents :: IO [Int]
  let x = fmap (\n -> (head n, length n)) . group . sort $ns
  let m = step' 256 $ initMap x
  let ans = foldr (+) 0 m
  print ans