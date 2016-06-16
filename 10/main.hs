{-
prime :: (Integral a, Eq a) => a -> Bool
prime 1 = False
prime n = null [x | x <- [2..floor (sqrt (fromIntegral n))], mod n x == 0]

primes = [x | x <- [1..2*10^6], prime x]
-}

import Data.Numbers.Primes

main = print $ sum $ takeWhile(<=2*10^6) primes
