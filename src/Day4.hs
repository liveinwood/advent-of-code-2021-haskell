module Day4 where

import Data.List.Split
import Data.Vector as V hiding (reverse, (++))
import GHC.Arr (badSafeIndex)

type N = (Int, Bool)

type Board = Vector (Vector N)

mark :: Int -> Board -> Board
mark n = V.map (V.map (\(i, b) -> if i == n then (i, True) else (i, b)))

allMarkedRowExist :: Board -> Bool
allMarkedRowExist = V.or . V.map (V.all snd)

allMarkedColumExist :: Board -> Bool
allMarkedColumExist vv = V.or $ V.foldl (V.zipWith (\b (n, b') -> b && b')) allTrueVector vv
  where
    allTrueVector = V.replicate (V.length $ V.head vv) True

win :: Board -> Bool
win vv = allMarkedRowExist vv || allMarkedColumExist vv

score :: Board -> Int
score = V.sum . V.map ((V.foldl (\n -> \(n', _) -> n + n') 0) . (V.filter (\(n, b) -> not b)))

makeBoard :: [[Int]] -> Board
makeBoard nn = V.fromList $ V.fromList <$> fmap (fmap (\n -> (n, False))) nn

makeBoards :: [String] -> [Board]
makeBoards ss = if Prelude.null ss then [] else (makeBoard $ (fmap (read :: String -> Int)) . words <$> Prelude.take 5 ss) : makeBoards (Prelude.drop 5 ss)

solve = do
  lines <- Prelude.filter (/= "") . lines <$> getContents
  let drawNumbers = (read :: String -> Int) <$> splitOn "," (Prelude.head lines)
  let boards = makeBoards $ Prelude.tail lines
  let (winNumber, winBoards) = go drawNumbers boards
  print $ winNumber * (Prelude.maximum . (Prelude.map score) $ winBoards)

go :: [Int] -> [Board] -> (Int, [Board])
go [] bs = error "never reach here"
go (n : ns) bs =
  let nextBoards = Prelude.map (mark n) bs
      winBoards = Prelude.filter win nextBoards
   in if Prelude.null winBoards then go ns nextBoards else (n, winBoards)