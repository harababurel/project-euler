import Data.List
import Data.Numbers.Primes (isPrime)

sol :: Int -> [Int]
sol n = filter isPrime $ map read $ permutations $ foldl1 (++) $ map show [1..n]

hasAChance :: Int -> Bool
hasAChance n = mod (div (n*(n+1)) 2) 3 /= 0

main = do
    print $ maximum $ foldl1 (++) $ map sol $ filter hasAChance [1..9]

-- Obs: only 4- and 7-pandigital numbers must be considered
-- because all others are divisible by 3
