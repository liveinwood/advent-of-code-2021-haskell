module Day9 where

import Data.Char
import Data.List
import Data.Set as S (Set, fromList, singleton, size, union, unions)
import Data.Vector as V (Vector, empty, fromList, head, ifilter, imap, length, (!), (!?))
import Debug.Trace

from2DList :: [[Int]] -> Vector (Vector Int)
from2DList = V.fromList . fmap V.fromList

at :: (Int, Int) -> Vector (Vector Int) -> Maybe Int
at (r, c) v = do
  row <- v !? r
  row !? c

adjacent :: (Int, Int) -> Vector (Vector Int) -> [Int]
adjacent (r, c) v = foldMap f [at p v | p <- [(r - 1, c), (r, c + 1), (r + 1, c), (r, c - 1)]]
  where
    f (Just x) = [x]
    f Nothing = []

higherPoints :: (Int, Int) -> Vector (Vector Int) -> [(Int, Int)]
higherPoints (r, c) v = [(r', c') | (r', c') <- [(r - 1, c), (r, c + 1), (r + 1, c), (r, c - 1)], isJust $ at (r', c') v, v ! r' ! c' > v ! r ! c, v ! r' ! c' < 9]
  where
    isJust (Just _) = True
    isJust Nothing = False

isLowPoint :: (Int, Int) -> Vector (Vector Int) -> Bool
isLowPoint (r, c) v = v ! r ! c < minimum (adjacent (r, c) v)

solve = do
  vec <- from2DList . fmap (fmap digitToInt) . lines <$> getContents
  let lowPoints = [vec ! r ! c | r <- [0 .. V.length vec - 1], c <- [0 .. (V.length . V.head $ vec) - 1], isLowPoint (r, c) vec]
  print $ sum . fmap (+ 1) $ lowPoints

dfs :: Vector (Vector Int) -> (Int, Int) -> Set (Int, Int)
dfs v p = singleton p `S.union` unions (fmap (dfs v) (higherPoints p v))

-- dfs v p = singleton (traceShow ("p = " <> show p) p) `S.union` unions (fmap (dfs v) (higherPoints p v))

solve' = do
  vec <- from2DList . fmap (fmap digitToInt) . lines <$> getContents
  let lowPoints = [(r, c) | r <- [0 .. V.length vec - 1], c <- [0 .. (V.length . V.head $ vec) - 1], isLowPoint (r, c) vec]
  print $ product . take 3 . sortBy (flip compare) . fmap (S.size . dfs vec) $ lowPoints
