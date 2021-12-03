module Day3 where

import Data.Char (digitToInt)

toList :: String -> [Int]
toList = fmap digitToInt

makeEmptyList :: Int -> [Int]
makeEmptyList n = replicate n 0

add :: [Int] -> [Int] -> [Int]
add = zipWith (\x y -> if y == 1 then x + 1 else x -1)

toDecimal :: [Int] -> Int
toDecimal xs = sum $ zipWith (*) (iterate (* 2) 1) (reverse xs)

solve :: IO ()
solve = do
  lists <- fmap toList . lines <$> getContents
  let empty = makeEmptyList . (length . head) $ lists
  let gamma = (\x -> if x >= 0 then 1 else 0) <$> foldl add empty lists
  let epsilon = (\x -> if x < 0 then 1 else 0) <$> foldl add empty lists
  print $ toDecimal gamma * toDecimal epsilon
