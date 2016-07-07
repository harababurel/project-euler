import Data.Array


shortestSquarePath a b c = minimum $ [squareDiagonal a (b+c)
                                     ,squareDiagonal b (a+c)
                                     ,squareDiagonal c (a+b)]
    where squareDiagonal a b = a^2 + b^2


isInt    x = x == (fromIntegral . floor) x
isSquare x = isInt $ sqrt $ fromIntegral x

nmax = 2000

sol :: Int -> Int
sol n = table ! n
    where table = (listArray (1,nmax) . map sol') [1..nmax]
          sol' 1 = 0
          sol' n = table ! (n-1) + length [(a, b, n) | a <- [1..n]
                                                     , b <- [a..n]
                                                     , isSquare (shortestSquarePath a b n)]

{-
search :: Int -> Int -> Int -> Int
search n lo hi
    | hi - lo <= 1 = hi
    | (sol mid) > n = search n lo mid
    | otherwise = search n mid hi
    where mid = div (lo+hi) 2
-}

takeWhileInclusive p [] = []
takeWhileInclusive p (x:xs)
    | p x = x:takeWhileInclusive p xs
    | otherwise = [x]

main = do
    --print $ search (10^6) 1 nmax
    print $ last $ takeWhileInclusive (\n -> sol n <= 10^6) [1..]
