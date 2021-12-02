module Day2 (module Control.Monad.State, Instruction (..), SubmarineState (..), parseInstruction, solve) where

import Control.Monad.State
import Data.Foldable (traverse_)

data Instruction = Forward Int | Up Int | Down Int deriving (Show)

newtype SubmarineState = SubmarineState (Int, Int)

parseInstruction :: String -> Instruction
parseInstruction s =
  let [d, n] = words s
   in case d of
        "forward" -> Forward (read n)
        "up" -> Up (read n)
        "down" -> Down (read n)
        -- _ -> Down 0
        _ -> error "unknown instruction."

solve :: [Instruction] -> State SubmarineState ()
solve = traverse_ execute

execute :: Instruction -> State SubmarineState ()
execute ins = do
  SubmarineState (h, d) <- get
  put $ nextSubmarineSate ins (SubmarineState (h, d))

nextSubmarineSate :: Instruction -> SubmarineState -> SubmarineState
nextSubmarineSate ins (SubmarineState (h, d)) =
  case ins of
    Forward hh -> SubmarineState (h + hh, d)
    Up dd -> SubmarineState (h, d - dd)
    Down dd -> SubmarineState (h, d + dd)
