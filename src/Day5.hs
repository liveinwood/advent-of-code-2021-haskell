module Day5 where

import Data.List.Split
import Data.Map as M

data P = P {_x :: Int, _y :: Int} deriving (Show)

makePoint :: String -> P
makePoint s =
  let [x, y] = splitOn "," s
   in P {_x = read x, _y = read y}

isHorizontal :: P -> P -> Bool
isHorizontal p q = _y p == _y q

isVertical :: P -> P -> Bool
isVertical p q = _x p == _x q

pointsOnLine :: P -> P -> [P]
pointsOnLine p q
  | _x p == _x q = [P {_x = _x p, _y = y} | y <- [(min (_y p) (_y q)) .. (max (_y p) (_y q))]]
  | _y p == _y q = [P {_x = x, _y = _y p} | x <- [(min (_x p) (_x q)) .. (max (_x p) (_x q))]]
  | otherwise = []

splitOnArrow :: String -> (String, String)
splitOnArrow s = let [p, q] = splitOn " -> " s in (p, q)

solve = do
  pairs <- fmap splitOnArrow . lines <$> getContents
  let points = fmap (\pair -> (makePoint $ fst pair, makePoint $ snd pair)) pairs
  print points

go :: [(P, P)] -> M.Map P Int -> M.Map P Int
go [] m = m
go (pair:pairs) m = if (isHorizontal (fst pair) (snd pair)) || (isVertical (fst pair) (snd pair)) then 