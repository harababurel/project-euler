import Data.List
import Data.Numbers.Primes

factorizations :: Int -> Int
factorizations n = 1 + div (product $ map (succ . length) $ group $ primeFactors n) 2

sol :: Int -> Int
sol n = factorizations (n^2)

takeWhileInclusive p (x:xs)
    | p x = x:takeWhileInclusive p xs
    | otherwise = [x]

sols = takeWhileInclusive (\x -> snd x < 1000) $ zip [1..] (map sol [1..])

main = do
    sequence $ map (print . head) $ group $ scanl1 (\x acc -> if snd x >= snd acc then x else acc) sols

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
