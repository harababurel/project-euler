import Data.Numbers.Primes (primeFactors)
import Data.List (group)

main = print $ head [n | n <- [1..],
                          distinctFactors n     == 4,
                          distinctFactors (n+1) == 4,
                          distinctFactors (n+2) == 4,
                          distinctFactors (n+3) == 4]
    where distinctFactors n = length $ group $ primeFactors n
