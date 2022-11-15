{-# LANGUAGE OverloadedStrings #-}
module Day12 where

import           Control.Monad            (forM_, guard)
import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Char8    as BS
import           Data.Char                (isUpper)
import           Data.List                (unfoldr)
import qualified Data.Map                 as M
import           Data.Maybe               (fromMaybe)
import           Data.Vector.Unboxed.Base (Vector (V_Bool))
import           Debug.Trace              (trace)

data Cave = Start | End | Big BS.ByteString | Small BS.ByteString deriving (Eq, Ord, Show)

type Graph = M.Map Cave [Cave]

type Path = [Cave]

dfs :: Graph -> Path -> Cave -> Int
dfs graph path End = 1
dfs graph path current = foldl (\x nc -> x + dfs graph (current:path) nc) 0 next
    where
        next :: [Cave]
        next = [c | c <- nextCaves, c `notElem` smallCaveInPath, c /= Start]

        nextCaves :: [Cave]
        nextCaves = fromMaybe [] (M.lookup current graph)

        smallCaveInPath :: [Cave]
        smallCaveInPath = filter isSmallCave path

        isSmallCave :: Cave -> Bool
        isSmallCave (Small _) = True
        isSmallCave _         = False

makeGraph :: IO Graph
makeGraph = do
    lines_ <- BS.lines <$> BS.getContents
    let pairs = concatMap makePairs lines_
    return $ M.fromListWith (++) pairs

    where
        makePairs :: BS.ByteString -> [(Cave, [Cave])]
        makePairs s = let [c, c'] = BS.split '-' s
                      in [(toCave c, [toCave c']), (toCave c', [toCave c])]

        toCave :: BS.ByteString -> Cave
        toCave "start" = Start
        toCave "end"   = End
        toCave s       = (if isUpper $ BS.head s then Big else Small) s

solve = do
    graph <- makeGraph
    print $ dfs graph [] Start
