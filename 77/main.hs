import Data.Array
import Data.Numbers.Primes

-- Same idea as 76

dp n x = table ! (n, x)
    where bnds = ((0,0), (n,n))
          table = listArray bnds . map dp' $ range bnds

          dp' (n, x)
            | not $ isPrime x = 0
            | x == n = 1
            | otherwise = sum [table ! ((n-x), z) | z <- takeWhile (<= min (n-x) x) primes]

ways n = sum $ map (dp n) [1..n-1]

main = print $ head $ filter (\x -> ways x > 5000) [1..]
