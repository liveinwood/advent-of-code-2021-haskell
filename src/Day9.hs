module Day9 where

import Data.Char
import Data.List
import Data.Vector as V (Vector, empty, fromList, head, ifilter, imap, length, (!), (!?))

from2DList :: [[Int]] -> Vector (Vector Int)
from2DList = fromList . fmap fromList

at :: (Int, Int) -> Vector (Vector Int) -> Maybe Int
at (r, c) v = do
  row <- v !? r
  row !? c

adjacent :: (Int, Int) -> Vector (Vector Int) -> [Int]
adjacent (r, c) v = foldMap f [at p v | p <- [(r - 1, c), (r, c + 1), (r + 1, c), (r, c - 1)]]
  where
    f (Just x) = [x]
    f Nothing = []

isLowPoint :: (Int, Int) -> Vector (Vector Int) -> Bool
isLowPoint (r, c) v = (v ! r ! c) < minimum (adjacent (r, c) v)

solve = do
  vec <- from2DList . fmap (fmap digitToInt) . lines <$> getContents
  let lowPoints = [vec ! r ! c | r <- [0 .. V.length vec - 1], c <- [0 .. (V.length . V.head $ vec) - 1], isLowPoint (r, c) vec]
  print $ sum . fmap (+ 1) $ lowPoints