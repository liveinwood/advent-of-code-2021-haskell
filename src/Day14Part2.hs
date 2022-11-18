{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Day14Part2 where

import qualified Data.ByteString.Char8 as BS
import qualified Data.List             as L
import qualified Data.Map              as M

type PolymerPair = (Char, Char)
type PolymerTemplate = [(PolymerPair, Int)]
type InsertionRule = M.Map PolymerPair Char

insert :: InsertionRule -> PolymerPair -> [PolymerPair]
insert rule (c1, c2) = case M.lookup (c1, c2) rule of
                        Just c  -> [(c1, c), (c, c2)]
                        Nothing -> [(c1, c2)]

stepN :: Int -> InsertionRule -> PolymerTemplate -> PolymerTemplate
stepN 0 rule templ = templ
stepN n rule templ = stepN (n - 1) rule (compress $ step rule templ)

step :: InsertionRule -> PolymerTemplate -> PolymerTemplate
step rule templ = do
    (pair, count) <- templ
    (, count) <$> insert rule pair

compress :: PolymerTemplate -> PolymerTemplate
compress templ = foldl help [] $ L.sort templ
    where
        help :: PolymerTemplate -> (PolymerPair, Int) -> PolymerTemplate
        help [] (pair, count) = [(pair, count)]
        help ((pair, count):templ) (pair', count') = if pair == pair' then
                                                        (pair, count + count'):templ
                                                     else
                                                        (pair', count'):(pair, count):templ

-- countPolymer :: String -> PolymerTemplate -> [(Char, Int)]
countPolymer :: String -> PolymerTemplate -> [Int]
countPolymer starterCode templ = map quotRoundUp $ foldl accumCount [] $ L.sort $ help templ
    where
        accumCount :: [(Char, Int)] -> (Char, Int) -> [(Char, Int)]
        accumCount [] (polymer, count) = [(polymer, count)]
        accumCount ((polymer, count):pp) (polymer', count') = if polymer == polymer' then
                                                                (polymer, count + count'):pp
                                                            else
                                                                (polymer', count'):(polymer, count):pp

        help :: PolymerTemplate -> [(Char, Int)]
        help templ = do
            ((c1, c2), count) <- templ
            [(c1, count), (c2, count)]

        quotRoundUp :: (Char, Int) -> Int
        quotRoundUp (c, i) = if even i
        then quot i 2 + if head starterCode == c && last starterCode == c
            then 1
            else 0
        else quot i 2 + 1
