module Main where

import Day1

main :: IO ()
main = do
  l <- fmap (read :: String -> Int) . lines <$> getContents
  print $ solve2 l