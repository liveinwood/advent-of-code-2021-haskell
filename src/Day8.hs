module Day8 where

import Control.Monad
import Data.Char (toUpper)
import Data.List.Split (splitOn)
import System.IO

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