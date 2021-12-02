module Day2 (module Control.Monad.State, Instruction (..), SubmarineState (..), parseInstruction, solve) where

import Control.Monad.State
import Data.Foldable (traverse_)

data Instruction = Forward Int | Up Int | Down Int deriving (Show)

-- (horizontal, depth, aim)
newtype SubmarineState = SubmarineState (Int, Int, Int)

parseInstruction :: String -> Instruction
parseInstruction s =
  let [d, n] = words s
   in case d of
        "forward" -> Forward (read n)
        "up" -> Up (read n)
        "down" -> Down (read n)
        _ -> error "unknown instruction."

solve :: [Instruction] -> State SubmarineState ()
solve = traverse_ execute

execute :: Instruction -> State SubmarineState ()
execute ins = do
  SubmarineState (h, d, a) <- get
  put $ nextSubmarineSate ins (SubmarineState (h, d, a))

nextSubmarineSate :: Instruction -> SubmarineState -> SubmarineState
nextSubmarineSate ins (SubmarineState (h, d, a)) =
  case ins of
    Forward hh -> SubmarineState (h + hh, d + hh * a, a)
    Up dd -> SubmarineState (h, d, a - dd)
    Down dd -> SubmarineState (h, d, a + dd)
