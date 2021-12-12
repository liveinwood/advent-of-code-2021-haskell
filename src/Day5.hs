module Day5 where

import Data.List.Split
import Data.Map as M
import Data.Maybe

data P = P {_x :: Int, _y :: Int} deriving (Show, Eq, Ord)

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

makePoints :: [(String, String)] -> [P]
makePoints spairs = do
  (p1, p2) <- (\pair -> (makePoint $ fst pair, makePoint $ snd pair)) <$> spairs
  if isVertical p1 p2 || isHorizontal p1 p2 then pointsOnLine p1 p2 else []

solve = do
  pairs <- fmap splitOnArrow . lines <$> getContents
  let points = makePoints pairs
  let m = Prelude.filter (\(_, c) -> c > 1) (M.toList . go $ points)
  print $ length m

go :: [P] -> M.Map P Int
go = Prelude.foldl f M.empty
  where
    f m p = let c = fromMaybe 0 (M.lookup p m) in M.insert p (c + 1) m