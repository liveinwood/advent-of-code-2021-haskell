{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Main where
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map              as M
import           Data.Monoid
import qualified Data.Vector           as V

-- import           Day1
-- import           Day10
-- import           Day12
import           Day14Part2
-- import           Day2
-- import           Day3
-- import           Day4
-- import           Day5
-- import           Day6
-- import           Day7
-- import           Day8

main :: IO ()
main = do
    let
        start = "NNSOFOCNHBVVNOBSBHCB"
        templ = map (,1) $ zip start (tail start) :: [((Char, Char), Int)]
        rule = M.fromList [(('H','N'), 'S'),(('F','K'), 'N'),(('C','H'), 'P'),(('V','P'), 'P'),(('V','V'), 'C'),(('P','B'), 'H'),(('C','P'), 'F'),(('K','O'), 'P'),(('K','N'), 'V'),(('N','O'), 'K'),(('N','F'), 'N'),(('C','O'), 'P'),(('H','O'), 'H'),(('V','H'), 'V'),(('O','V'), 'C'),(('V','S'), 'F'),(('P','K'), 'H'),(('O','S'), 'S'),(('B','F'), 'S'),(('S','N'), 'P'),(('N','K'), 'N'),(('S','V'), 'O'),(('K','B'), 'O'),(('O','N'), 'O'),(('F','N'), 'H'),(('F','O'), 'N'),(('K','V'), 'S'),(('C','S'), 'C'),(('V','O'), 'O'),(('S','P'), 'O'),(('V','K'), 'H'),(('K','P'), 'S'),(('S','K'), 'N'),(('N','C'), 'B'),(('P','N'), 'N'),(('H','V'), 'O'),(('H','S'), 'C'),(('C','N'), 'N'),(('O','O'), 'V'),(('F','F'), 'B'),(('V','C'), 'V'),(('H','K'), 'K'),(('C','C'), 'H'),(('B','O'), 'H'),(('S','C'), 'O'),(('H','H'), 'C'),(('B','V'), 'P'),(('O','B'), 'O'),(('F','C'), 'H'),(('P','O'), 'C'),(('F','V'), 'C'),(('B','K'), 'F'),(('H','B'), 'B'),(('N','H'), 'P'),(('K','F'), 'N'),(('B','P'), 'H'),(('K','K'), 'O'),(('O','H'), 'K'),(('C','B'), 'H'),(('C','K'), 'C'),(('O','K'), 'H'),(('N','N'), 'F'),(('V','F'), 'N'),(('S','O'), 'K'),(('O','P'), 'F'),(('N','P'), 'B'),(('F','S'), 'S'),(('S','H'), 'O'),(('F','P'), 'O'),(('S','F'), 'V'),(('H','F'), 'N'),(('K','C'), 'K'),(('S','B'), 'V'),(('F','H'), 'N'),(('S','S'), 'C'),(('B','B'), 'C'),(('N','V'), 'K'),(('O','C'), 'S'),(('C','V'), 'N'),(('H','C'), 'P'),(('B','C'), 'N'),(('O','F'), 'K'),(('B','H'), 'N'),(('N','S'), 'K'),(('B','N'), 'F'),(('P','C'), 'C'),(('C','F'), 'N'),(('H','P'), 'F'),(('B','S'), 'O'),(('P','F'), 'S'),(('P','V'), 'B'),(('K','H'), 'K'),(('V','N'), 'V'),(('N','B'), 'N'),(('P','H'), 'V'),(('K','S'), 'B'),(('P','P'), 'V'),(('P','S'), 'C'),(('V','B'), 'N'),(('F','B'), 'N')]

    let counts = countPolymer start $ stepN 40 rule templ
    print $ maximum counts - minimum counts
    -- print $ compress [(('N','B'),1),(('B','C'),1),(('C','N'),1),(('C','C'),1),(('N','B'),1),(('B','B'),1),(('B','C'),1),(('B','B'),1),(('H','C'),1),(('C','B'),1),(('C','B'),1),(('B','H'),1)]
    -- print $ compress $ step rule [(('N', 'N'), 1), (('N', 'C'), 1), (('C', 'B'), 1), (('X', 'N'), 1)]
