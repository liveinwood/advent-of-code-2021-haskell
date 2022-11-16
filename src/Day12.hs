{-# LANGUAGE OverloadedStrings #-}
module Day12 where

import           Control.Monad            (forM_, guard)
import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Char8    as BS
import           Data.Char                (isUpper)
import           Data.List                (group, sort, unfoldr)
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



------------------------------ part 2 ---------------------------
type Visited = M.Map Cave Int

initVisted :: [Cave] -> Visited
initVisted cs = M.fromList $ zip cs (repeat 0)


dfs' :: Graph -> Path -> Int
dfs' graph path = if validPath path then go graph path else 0
    where
        validPath :: Path -> Bool
        validPath path = let
                            smallCaves = sort $ filter isSmallCave path
                         in
                            null smallCaves || (all (\cs -> length cs <= 2) (group smallCaves) && (length ( filter (\cs -> length cs == 2) (group smallCaves)) <= 1 ))


        go :: Graph -> Path -> Int
        go graph path = if head path == End then 1 else foldl (\x c -> x + dfs' graph (c:path)) 0 nextCaves

        nextCaves :: [Cave]
        nextCaves = filter (not . isStart) $ fromMaybe [] (M.lookup (head path) graph)

        isSmallCave :: Cave -> Bool
        isSmallCave (Small _) = True
        isSmallCave _         = False

        isStart Start = True
        isStart _     = False
solve' = do
    graph <- makeGraph
    print $ dfs' graph [Start]
