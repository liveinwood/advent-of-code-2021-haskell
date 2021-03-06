module Day10 where

import Data.List

-- Part 1
data Result = Incomplete | Corrupted Char | Ok deriving (Show)

type Stack = String

isOpen :: Char -> Bool
isOpen c = c == '[' || c == '(' || c == '{' || c == '<'

match :: Char -> Char -> Bool
match '[' ']' = True
match '{' '}' = True
match '(' ')' = True
match '<' '>' = True
match _ _ = False

check :: Stack -> String -> Result
check [] (c : cs) = if isOpen c then check [c] cs else Corrupted c
check stack (c : cs)
  | isOpen c = check (c : stack) cs
  | match (head stack) c = check (tail stack) cs
  | otherwise = Corrupted c
check stack [] = if null stack then Ok else Incomplete

score :: [Result] -> Int
score = foldl f 0
  where
    f n Incomplete = n
    f n Ok = n
    f n (Corrupted c) = n + getScore c
    getScore ')' = 3
    getScore ']' = 57
    getScore '}' = 1197
    getScore '>' = 25137
    getScore _ = error "never reach here!"

solve :: IO ()
solve = do
  lines <- lines <$> getContents
  print $ score . fmap (check []) $ lines

-- Part 2

data Result' = Incomplete' String | Corrupted' Char | Ok' deriving (Show)

toClosing :: String -> String
toClosing = fmap f
  where
    f '(' = ')'
    f '[' = ']'
    f '{' = '}'
    f '<' = '>'
    f _ = error "never reach here!"

check' :: Stack -> String -> Result'
check' [] (c : cs) = if isOpen c then check' [c] cs else Corrupted' c
check' stack (c : cs)
  | isOpen c = check' (c : stack) cs
  | match (head stack) c = check' (tail stack) cs
  | otherwise = Corrupted' c
check' stack [] = if null stack then Ok' else Incomplete' $ toClosing stack

score' :: Result' -> [Int]
score' Ok' = []
score' (Corrupted' _) = []
score' (Incomplete' cs) = [foldl f 0 cs]
  where
    f n c = n * 5 + getPoint c
    getPoint ')' = 1
    getPoint ']' = 2
    getPoint '}' = 3
    getPoint '>' = 4
    getPoint _ = error "never reach here!"

solve' :: IO ()
solve' = do
  lines <- lines <$> getContents
  let results = sort $ concatMap (score' . check' []) lines
  print $ head $ drop (length results `div` 2) results
