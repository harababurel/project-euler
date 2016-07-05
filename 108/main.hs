import Data.List
import Data.Numbers.Primes

factorizations :: Integer -> Integer
factorizations n = 1 + toInteger (div (product $ map (succ . length) $ group $ primeFactors n) 2)

sol :: Integer -> Integer
sol n = factorizations (n^2)

candidates = [2^a * 3^b * 5^c * 7^d * 11^e * 13^f | a <- [0..2],
                                                    b <- [0..2],
                                                    c <- [0..2],
                                                    d <- [0..2],
                                                    e <- [0..2],
                                                    f <- [0..2]]

sols = filter (\x -> sol x > 1000) candidates

main = do
    print $ minimum sols

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
