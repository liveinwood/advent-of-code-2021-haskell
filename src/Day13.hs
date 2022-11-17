module Day13 (foldLeft2dVAt, foldUp2dVAt, visibleDotCount, readInput, update2dVector, make2dVector) where

import qualified Data.ByteString.Char8 as BS
import           Data.Maybe
import           Data.Monoid
import qualified Data.Vector           as V


make2dVector :: BS.ByteString -> V.Vector (V.Vector (Sum Int))
make2dVector ss = let
                    (tuples, (maxX, maxY)) = (readInput . BS.lines) ss
                    initVector = V.replicate (maxY + 1) (V.replicate (maxX + 1) mempty)
                  in
                    foldl (\vv tpl -> update2dVector vv tpl (Sum 1)) initVector tuples

update2dVector :: V.Vector (V.Vector a) -> (Int, Int) -> a -> V.Vector (V.Vector a)
update2dVector vv (x, y) a = let
                                v' = V.update (vv V.! y) (V.fromList [(x, a)])
                             in
                                V.update vv (V.fromList [(y, v')])

readInput :: [BS.ByteString] -> ([(Int, Int)], (Int, Int))
readInput ss = readInput' ss [] (0, 0)

readInput' :: [BS.ByteString] -> [(Int, Int)] -> (Int, Int) -> ([(Int, Int)], (Int, Int))
readInput' [] tuples maxXY = (tuples, maxXY)
readInput' (s:ss) tuples (maxX, maxY) = case readXY s of
                                            Just (x, y) -> readInput' ss ((x, y):tuples) (maximum [x, maxX], maximum [y, maxY])
                                            Nothing     -> error "ERROR!"

readXY :: BS.ByteString -> Maybe (Int, Int)
readXY s = do
    (x, s') <- BS.readInt s
    (y, _) <- BS.readInt (BS.drop 1 s')
    return (x, y)

foldLeft2dVAt :: Monoid a => Int -> V.Vector (V.Vector a) -> V.Vector (V.Vector a)
foldLeft2dVAt n = V.map (foldLeftVAt n)


foldLeftVAt :: Monoid a => Int -> V.Vector a -> V.Vector a
foldLeftVAt n v = let
                    (p, q) = splitVAt n v
                  in
                    V.zipWith mappend p q

splitVAt :: Monoid a => Int -> V.Vector a -> (V.Vector a, V.Vector a)
splitVAt n v = let
                p = V.take n v
                q = V.reverse $ V.drop (n + 1) v
            in
                (V.replicate (V.length q - V.length p) mempty V.++ p
                ,V.replicate (V.length p - V.length q) mempty V.++ q)


foldUp2dVAt :: Monoid a => Int -> V.Vector (V.Vector a) -> V.Vector (V.Vector a)
foldUp2dVAt n v = let
                    (p, q) = split2dVAt n v
                in
                    V.zipWith (V.zipWith mappend) p q


split2dVAt :: Monoid a => Int -> V.Vector (V.Vector a) -> (V.Vector (V.Vector a), V.Vector (V.Vector a))
split2dVAt n v = let p = V.take n v
                     q = V.reverse $ V.drop (n + 1) v
                 in (V.replicate (V.length q - V.length p) memptyV V.++ p
                    ,V.replicate (V.length p - V.length q) memptyV V.++ q)
                where
                    memptyV :: Monoid a => V.Vector a
                    memptyV = V.replicate (V.length . V.head $ v) mempty

visibleDotCount :: (Eq a, Monoid a) => V.Vector (V.Vector a) -> Int
visibleDotCount v = V.sum $ V.map (V.length . V.filter (/= mempty)) v
