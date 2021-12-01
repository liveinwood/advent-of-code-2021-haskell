module Day1 where

-- part 1

solve :: [Int] -> Int
solve xs = solve' [] xs 0

solve' :: [Int] -> [Int] -> Int -> Int
solve' (x : xs) (y : ys) acc = if x < y then solve' (y : x : xs) ys (acc + 1) else solve' (y : x : xs) ys acc
solve' [] [] _ = 0
solve' (x : xs) [] acc = acc
solve' [] (y : ys) _ = solve' [y] ys 0

-- part 2

solve2 :: [Int] -> Int
solve2 = solve . (fmap sum) . slice

slice :: [Int] -> [[Int]]
slice xs = if (length xs) >= 3 then (take 3 xs) : slice (tail xs) else []