import Data.MemoCombinators.Class (memoize)
import Data.MemoUgly
import Data.Int

-- Needs better memoization :-??

data Letter = A | O | L
--data Streak = 3 | 1 | 2

sol = dp
    where dp :: Int8 -> Letter -> Int8 -> Bool -> Int32
          dp 0  _  _ _ = 0
          dp 1 A 1 False = 1
          dp 1 O 0 False = 1
          dp 1 L 0 True = 1
          dp 1 _ _ _ = 0

          dp i O streak hasL
            | streak /= 0 = 0
            | otherwise = sum [(sol) (i-1) A lastStreak hasL | lastStreak <- [0..2]] +
                          sum [(sol) (i-1) lastJ 0 hasL | lastJ <- [L,O]]

          dp i L streak hasL
            | streak /= 0 = 0
            | not hasL = 0
            | otherwise = sum [sol (i-1) A lastStreak False | lastStreak <- [0..2]] +
                          sol (i-1) O 0 False

          dp i A streak hasL
            | streak < 1 = 0    -- by adding an A, the streak is at least 1
            | streak > 2 = 0    -- can't put more than two consecutive As
            | streak == 1 = sum [(sol) (i-1) lastJ 0 hasL | lastJ <- [L,O]]
            | otherwise = sum [(sol) (i-1) A (streak-1) hasL]

main = do
    line <- getLine
    let n = read line :: Int8

    print $ sum $ [sum [sol n O 0 hasL | hasL <- [False,True]]
                ,sum [sol n A streak hasL | streak <- [1..2], hasL <- [False,True]]
                ,sol n L 0 True]
