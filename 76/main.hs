import Data.Array

{- dp n x = number of ways of writing n as a sum of positive integers
 -          that are written in ascending order, and the last one is x
 -
 - dp n n = 1 (write the number n by itself)
 - Otherwise, assume we want to add an integer x to the sum.
 -
 - 1. The previous sum must have been (n-x)
 - 2. The last integer of the previous sum must have been:
 -    * at least 1
 -    * at most x (ascending order)
 -    * at most n-x (couldn't have been larger than the sum itself)
 -
 - Therefore, dp n x = sum [dp (n-x) [1..min (n-x x)]]
 -}

dp n x = table ! (n, x)
    where bnds = ((0,0), (n,n))
          table = listArray bnds . map dp' $ range bnds

          dp' (n, x)
            | x == n = 1
            | otherwise = sum [table ! ((n-x), z) | z <- [1..min (n-x) x]]

ways n = sum $ map (dp n) [1..n-1]

main = print $ ways 100
