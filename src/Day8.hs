module Day8 where

import Control.Monad
import Data.Char (toUpper)
import Data.List
import Data.List.Split (splitOn)
import Data.Map (Map, fromList, (!))
import qualified Data.Set as S
import System.IO

-- Part 1
solve = go 0 >>= print

go :: Int -> IO Int
go n = do
  done <- isEOF
  if done
    then return n
    else
      ( do
          line <- getLine
          let [l, r] = splitOn "|" line
          let rwords = words r
          let c = length $ filter p rwords
          go (n + c)
      )
  where
    p s = length s == 2 || length s == 4 || length s == 3 || length s == 7

-- Par 2
diff :: String -> String -> Int
diff s t = S.size $ S.difference (S.fromList s) (S.fromList t)

getValue :: Map String Int -> String -> Int
getValue m s = m ! (sort s)

makeMap :: [String] -> Map String Int
makeMap ss = fromList $ zipWith (\a b -> (sort a, b)) [zero, one, two, three, four, five, six, seven, eight, nine] [0 .. 9]
  where
    one = getStringOfLength 2 ss
    four = getStringOfLength 4 ss
    seven = getStringOfLength 3 ss
    eight = getStringOfLength 7 ss
    six = head [s | s <- getStringsOfLength 6 ss, diff one s == 1]
    nine = head [s | s <- getStringsOfLength 6 ss, diff four s == 0]
    zero = head [s | s <- getStringsOfLength 6 ss, s /= six, s /= nine]
    three = head [s | s <- getStringsOfLength 5 ss, diff one s == 0]
    five = head [s | s <- getStringsOfLength 5 ss, s /= three, diff nine s == 1]
    two = head [s | s <- getStringsOfLength 5 ss, s /= three, s /= five]
    getStringsOfLength n = filter ((== n) . length)
    getStringOfLength n = head . filter ((== n) . length)

go' :: Int -> IO Int
go' acc = do
  done <- isEOF
  if done
    then return acc
    else do
      strings <- splitOn "|" <$> getLine
      let [l, [d1, d2, d3, d4]] = fmap words strings
      let map = makeMap l
      let d = (getValue map d1) * 1000 + (getValue map d2) * 100 + (getValue map d3) * 10 + (getValue map d4)
      go' (acc + d)

solve' = go' 0 >>= print
