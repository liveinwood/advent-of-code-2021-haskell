module Main where

import Day1
import Day2

main :: IO ()
main = do
  ins <- fmap parseInstruction . lines <$> getContents
  let (_, SubmarineState (h, d, a)) = runState (Day2.solve ins) (SubmarineState (0, 0, 0))
  print (h * d)
