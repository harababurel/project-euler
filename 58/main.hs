import Data.Numbers.Primes
import Data.Ratio

deltas :: [Int]
deltas = foldr1 (++) $ map (replicate 4) [2,4..]
xs = scanl (+) 1 deltas

numberOfPrimes :: Int -> Int
numberOfPrimes n = length $ filter isPrime $ take (2*n-1) xs
ratio n = (numberOfPrimes n) % (2*n-1)

search :: Int -> Int -> Int
search lo hi
    | hi - lo   <= 1    = hi
    | ratio mid <= 1%10 = search lo mid
    | otherwise         = search mid hi
    where mid = div (lo+hi) 2

main = do
    print $ search 20000 30000
