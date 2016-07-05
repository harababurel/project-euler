import Data.List
import Data.Numbers.Primes

factorizations :: Integer -> Integer
factorizations n = 1 + toInteger (div (product $ map (succ . length) $ group $ primeFactors n) 2)

sol :: Integer -> Integer
sol n = factorizations (n^2)

takeWhileInclusive p (x:xs)
    | p x = x:takeWhileInclusive p xs
    | otherwise = [x]

sols = takeWhileInclusive (\x -> snd x < 4000000) $ zip [1..] (map sol [1..])

combine xs = product $ zipWith (\a b -> a^b) primes xs

best = 10^16
candidates = filter (<= best) [2^a * 3^b * 5^c * 7^d * 11^e * 13^f * 17^g * 19^h * 23^i * 29^j * 31^k * 37^l |
                                      a <- takeWhile (\x -> combine [x] <= best) [3..],
                                      b <- takeWhile (\x -> combine [a,x] <= best) [3..],
                                      c <- takeWhile (\x -> combine [a,b,x] <= best) [2..],
                                      d <- takeWhile (\x -> combine [a,b,c,x] <= best) [1..],
                                      e <- takeWhile (\x -> combine [a,b,c,d,x] <= best) [1..],
                                      f <- takeWhile (\x -> combine [a,b,c,d,e,x] <= best) [1..],
                                      g <- takeWhile (\x -> combine [a,b,c,d,e,f,x] <= best) [1..],
                                      h <- takeWhile (\x -> combine [a,b,c,d,e,f,g,x] <= best) [1..],
                                      i <- takeWhile (\x -> combine [a,b,c,d,e,f,g,h,x] <= best) [1..],
                                      j <- takeWhile (\x -> combine [a,b,c,d,e,f,g,h,i,x] <= best) [1..],
                                      k <- takeWhile (\x -> combine [a,b,c,d,e,f,g,h,i,j,x] <= best) [1..],
                                      l <- takeWhile (\x -> combine [a,b,c,d,e,f,g,h,i,j,k,x] <= best) [1..]]

main = do
    print $ minimum $ filter (\x -> snd x > 4*10^6) $ map (\x -> (x, sol x)) candidates
    --sequence $ map (print . head) $ group $ scanl1 (\x acc -> if fst x <= fst acc then x else acc) $ filter (\x -> snd x > 4*10^6) $ map (\x -> (x, sol x)) candidates

{- 1/x + 1/y = 1/n
 -
 - We must have x, y > n
 - Let x = n + a
 -     y = n + b
 -
 - 1/(n+a) + 1/(n+b) = 1/n
 - (n+b)+(n+a) = (n+a)*(n+b)/n
 - (2n+a+b)*n = n^2 + an + bn + ab
 - 2n^2 + an + bn = n^2 + an + bn + ab
 - n^2 = ab
 -
 - # of solutions = # of ways of factorizing n^2
 -}
