import Data.Numbers.Primes

partialSums :: [Int]
partialSums = scanl (+) 0 (takeWhile (<=4000) primes)

{-
sol = maximum [(b-a, diff a b) | a <- [0..length partialSums - 1],
                                 b <- [0..length partialSums - 1],
                                 a <= b,
                                 diff a b < 10^6,
                                 isPrime $ diff a b]
    where diff a b = (partialSums !! b) - (partialSums !! a)
-}
-- ^ this is slow

sol :: Int -> Int -> (Int, Int)
sol a b
    | b >= length partialSums = (0, 0)                            -- got to the end of my primes? stop
    | diff a b > 10^6         = sol (a+1) (b-1)                   -- last added element makes difference too large? remove it and shift the sequence to the right
    | isPrime $ diff a b      = max (b-a, diff a b) (sol a (b+1)) -- difference is prime? make it a candidate + try adding another prime
    | otherwise               = sol a (b+1)                       -- difference not prime? try adding another prime
    where diff a b = (partialSums !! b) - (partialSums !! a)

main = print $ sol 0 0
