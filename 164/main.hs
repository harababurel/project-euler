import Data.Array (range, listArray, (!))

dp :: Int -> Int -> Int -> Int -> Int
dp i a b c = table ! (i,a,b,c)
    where table = listArray bnds . map dp' $ range bnds
          bnds = ((0,0,0,0),(20,9,9,9))

          dp' (1,a,b,c) = if a == 0 && b == 0 &&     c /= 0 then 1 else 0
          dp' (2,a,b,c) = if a == 0 && b /= 0 &&   b+c <= 9 then 1 else 0
          dp' (3,a,b,c) = if a /= 0 &&           a+b+c <= 9 then 1 else 0

          dp' (i,a,b,c) = if a+b+c > 9 then 0
                                       else sum [table ! (i-1,x,a,b) | x <- [(if i == 4 then 1 else 0)..9]]

main = print $ sum [dp 20 a b c | (a,b,c) <- range ((0,0,0),(9,9,9))]
