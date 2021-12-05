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
