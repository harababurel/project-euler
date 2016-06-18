import Data.MemoCombinators.Class (memoize)
import Data.MemoUgly
import Data.Int
import Data.Array

-- Ugly but fast

-- a = 0 :: Int
-- o = 1 :: Int
-- l = 2 :: Int

dp :: Int -> Int -> Int -> Int -> Int
dp n letter absences hasL = table ! (n, letter, absences, hasL)
    where bnds = ((0,0,0,0), (n,2,2,1))
          table = listArray bnds . map dp' $ range bnds

          dp' :: (Int, Int, Int, Int) -> Int
          dp' (0, _, _, _) = 0
          dp' (1, 0, 1, 0) = 1
          dp' (1, 1, 0, 0) = 1
          dp' (1, 2, 0, 1) = 1
          dp' (1, _, _, _) = 0

          dp' (i, 1, absences, hasL)
            | absences /= 0 = 0
            | otherwise     = sum [table ! (pred i, 0,     lastAbsences, hasL) | lastAbsences <- [0,1,2]] +
                              sum [table ! (pred i, lastJ, 0,            hasL) | lastJ <- [1,2]]

          dp' (i, 2, absences, hasL)
            | absences /= 0 = 0
            | hasL == 0     = 0
            | otherwise     = sum [table ! (pred i, 0, lastAbsences, 0) | lastAbsences <- [0,1,2]] +
                                   table ! (pred i, 1, 0,            0)

          dp' (i, 0, absences, hasL)
            | absences == 0 = 0    -- by adding an A, the absences is at least 1
            | absences == 1 = sum [table ! (pred i, lastJ, 0,             hasL) | lastJ <- [1,2]]
            | otherwise     = sum [table ! (pred i, 0,     pred absences, hasL)]

main = do
    line <- getLine
    let n = read line :: Int

    print $ sum $ [sum [dp n 1 0        hasL | hasL <- [0,1]]
                  ,sum [dp n 0 absences hasL | absences <- [1,2], hasL <- [0,1]]
                  ,     dp n 2 0        1]
