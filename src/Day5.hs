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

isDiagonal :: P -> P -> Bool
isDiagonal p q = abs (_x p - _x q) == abs (_y p - _y q)

pointsOnLine :: P -> P -> [P]
pointsOnLine p q
  | _x p == _x q = [P {_x = _x p, _y = y} | y <- [(min (_y p) (_y q)) .. (max (_y p) (_y q))]]
  | _y p == _y q = [P {_x = x, _y = _y p} | x <- [(min (_x p) (_x q)) .. (max (_x p) (_x q))]]
  | _x p < _x q = zipWith (\x y -> P {_x = x, _y = y}) [(_x p) .. (_x q)] (iterate (\y -> if _y p > _y q then y - 1 else y + 1) (_y p))
  | _x p > _x q = zipWith (\x y -> P {_x = x, _y = y}) [(_x q) .. (_x p)] (iterate (\y -> if _y q > _y p then y - 1 else y + 1) (_y q))
  | otherwise = error "never reach here"

splitOnArrow :: String -> (String, String)
splitOnArrow s = let [p, q] = splitOn " -> " s in (p, q)

makePoints :: (P -> P -> Bool) -> [(String, String)] -> [P]
makePoints predicate spairs = do
  (p1, p2) <- (\pair -> (makePoint $ fst pair, makePoint $ snd pair)) <$> spairs
  if predicate p1 p2 then pointsOnLine p1 p2 else []

predicate :: P -> P -> Bool
predicate p1 p2 = isVertical p1 p2 || isHorizontal p1 p2

predicate' :: P -> P -> Bool
predicate' p1 p2 = isVertical p1 p2 || isHorizontal p1 p2 || isDiagonal p1 p2

solve predicate = do
  pairs <- fmap splitOnArrow . lines <$> getContents
  let points = makePoints predicate pairs
  let m = Prelude.filter (\(_, c) -> c > 1) (M.toList . go $ points)
  print $ length m

go :: [P] -> M.Map P Int
go = Prelude.foldl f M.empty
  where
    f m p = let c = fromMaybe 0 (M.lookup p m) in M.insert p (c + 1) m