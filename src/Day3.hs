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

-- part 2

elementsAt :: Int -> Vector (Vector Int) -> Vector Int
elementsAt n = V.map (! n)

mostCommonAt :: Int -> Vector (Vector Int) -> Int
mostCommonAt i vv =
  let t = V.foldl (\x y -> if y == 1 then x + 1 else x - 1) 0 (elementsAt i vv)
   in if t >= 0 then 1 else 0

leastCommonAt :: Int -> Vector (Vector Int) -> Int
leastCommonAt i vv =
  let t = V.foldl (\x y -> if y == 1 then x + 1 else x - 1) 0 (elementsAt i vv)
   in if t >= 0 then 0 else 1

filter' :: Int -> Int -> Vector (Vector Int) -> Vector (Vector Int)
filter' i n = V.filter (\v -> v ! i == n)

oxygen :: Vector (Vector Int) -> Int -> Vector Int
oxygen vv i =
  if V.length vv == 1
    then V.head vv
    else oxygen (filter' i (mostCommonAt i vv) vv) (i + 1)

co2 :: Vector (Vector Int) -> Int -> Vector Int
co2 vv i =
  if V.length vv == 1
    then V.head vv
    else co2 (filter' i (leastCommonAt i vv) vv) (i + 1)

solve' :: IO ()
solve' = do
  vectors <- V.fromList . fmap toVector . lines <$> getContents
  let oxygenRating = toDecimal $ oxygen vectors 0
  let co2Rating = toDecimal $ co2 vectors 0
  print $ oxygenRating * co2Rating
