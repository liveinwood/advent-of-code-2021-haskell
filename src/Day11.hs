{-# LANGUAGE BangPatterns #-}

module Day11 where

import Control.Monad ( guard, forM_ )
import qualified Data.Map        as M
import Data.List (unfoldr)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString (ByteString)

type Energy = Int
type Coordinate = (Int, Int)
type Grid = M.Map Coordinate Energy


-- 1  1  1  1  1
-- 1  9  9  9  1
-- 1  9  1  9  1
-- 1  9  9  9  1
-- 1  1  1  1  1

--       ↓ step1  e = e + 1

-- 2  2  2  2  2
-- 2 10 10 10  2
-- 2 10  2 10  2
-- 2 10 10 10  2
-- 2  2  2  2  2

--       ↓ step2  e = if e == 0 || e > 9 then 0 else e + フラッシュした(e > 9)隣接座標数

-- 3  4  5  4  3
-- 4  0  0  0  4
-- 5  0 10  0  5
-- 4  0  0  0  4
-- 3  4  5  4  3

--       ↓ step2  e = if e == 0 || e > 9 then 0 else e + フラッシュした(e > 9)隣接座標数

-- 3  4  5  4  3
-- 4  0  0  0  4
-- 5  0  0  0  5
-- 4  0  0  0  4
-- 3  4  5  4  3

solve :: Int -> (Int, Grid) -> (Int, Grid)
solve cnt (!n, !g) = if cnt <= 0 then (n, g) else solve (cnt - 1) (step (n, g))

step :: (Int, Grid) -> (Int, Grid)
step (n, g) =
    let g' = (step2 . step1) g
    in (n + flashCount g' , g')

step1 :: Grid -> Grid
step1 = M.map (+ 1)

step2 :: Grid -> Grid
step2 g =
    let
        g' = M.mapWithKey (f g) g
    in
        if g' == g then g' else step2 g'
    where
        f :: Grid -> Coordinate -> Energy -> Energy
        f g c e = if e == 0 || e > 9 then 0 else e + adjacentFlashCount g c

-- フラッシュした隣接座標数
adjacentFlashCount :: Grid -> Coordinate -> Int
adjacentFlashCount g c = length $ filter (> 9) $ adjacentEnergy g c

-- 隣接座標のエネルギー
adjacentEnergy :: Grid -> Coordinate -> [Energy]
adjacentEnergy g c = foldMap
                    (\c' -> case M.lookup c' g of
                                Just e  -> [e]
                                Nothing -> []
                    ) (adjacent g c)

-- フラッシュした座標数
flashCount :: Grid -> Int
flashCount g = M.size $ M.filter (== 0) g

-- 隣接座標
adjacent :: Grid -> Coordinate -> [Coordinate]
adjacent g (x, y) = do
    x' <- [-1, 0, 1]
    y' <- [-1, 0, 1]
    guard $ (x' /= 0 || y' /= 0) && M.member (x, y) g
    pure (x + x', y + y')

gridSize :: Grid -> Int
gridSize g = maximum $ map fst $ M.keys g

printGrid :: Grid -> IO ()
printGrid g = do
    let size = gridSize g
    forM_ [0..size] $ \x -> do
        forM_ [0..size] $ \y -> do
            putStr $ maybe "" show (M.lookup (x, y) g)
        putStr "\n"
    putStr "\n"

makeGrid :: IO Grid
makeGrid = do
    lines_ <- BS.lines <$> BS.getContents
    -- print $ toIntList $ BS.pack "11111"
    let
        ins = map toIntList lines_ :: [[Int]]
        pairs = concat $ zipWith (\x row -> zipWith (\y col -> ((x, y), col)) [0..] row) [0..] ins :: [((Int, Int), Int)]
    return $ M.fromList pairs

toIntList :: ByteString -> [Int]
toIntList s = if BS.null s then [] else case BS.readInt (BS.take 1 s) of
                                        Just (n, _) -> n : toIntList (BS.tail s)
                                        Nothing -> []

-------------------------------------- part 2 -----------------------------------------------------------

-- 全座標がフラッシュしたか
allFlash :: Grid -> Bool
allFlash g = flashCount g == (gridSize g + 1) * (gridSize g + 1)


solve2 :: Int -> Grid -> Int
solve2 cnt !g = if allFlash g then cnt else solve2 (cnt + 1) (snd (step (0, g)))
