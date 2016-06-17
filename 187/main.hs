import Data.Numbers.Primes (primes)

pairableWith :: Int -> Int
pairableWith x = length $ filter (>=x) $ takeWhile (\y -> x*y < 10^8) primes

sol :: Int
sol = sum $ takeWhile (>0) $ map pairableWith $ takeWhile (<10^8) primes

main = print $ sol
