module Day3 where

import Data.Char (digitToInt)
import Data.Vector as V

toVector :: String -> Vector Int
toVector s = fromList $ fmap digitToInt s

makeEmptyVector :: Int -> Vector Int
makeEmptyVector n = V.replicate n 0

add :: Vector Int -> Vector Int -> Vector Int
add = V.zipWith (\x y -> if y == 1 then x + 1 else x -1)

toDecimal :: Vector Int -> Int
toDecimal xs = V.sum $ V.zipWith (*) (V.iterateN (V.length xs) (* 2) 1) (V.reverse xs)

solve :: IO ()
solve = do
  lists <- V.fromList . fmap toVector . lines <$> getContents
  let empty = makeEmptyVector . (V.length . V.head) $ lists
  let gamma = (\x -> if x >= 0 then 1 else 0) <$> V.foldl add empty lists
  let epsilon = (\x -> if x < 0 then 1 else 0) <$> V.foldl add empty lists
  print $ toDecimal gamma * toDecimal epsilon
