import Data.Numbers.Primes

good :: Int -> Int -> Bool
good a b = good' a b && good' b a
    where good' a b = isPrime (read (show a ++ show b) :: Int)

sol :: Int -> [(Int, Int, Int, Int, Int)]
sol n = [(primes!!a, primes!!b, primes!!c, primes!!d, primes!!e) | a <- [1..n],
                                                                   b <- [a+1..n],
                                                                   good (primes!!a) (primes!!b),
                                                                   c <- [b+1..n],
                                                                   good (primes!!b) (primes!!c),
                                                                   good (primes!!a) (primes!!c),
                                                                   d <- [c+1..n],
                                                                   good (primes!!c) (primes!!d),
                                                                   good (primes!!b) (primes!!d),
                                                                   good (primes!!a) (primes!!d),
                                                                   e <- [d+1..n],
                                                                   good (primes!!d) (primes!!e),
                                                                   good (primes!!c) (primes!!e),
                                                                   good (primes!!b) (primes!!e),
                                                                   good (primes!!a) (primes!!e)]

tupleSum (a, b, c, d, e) = show (a,b,c,d,e) ++ " -> " ++ show (a+b+c+d+e)
main = putStrLn $ tupleSum $ head $ sol 1050
