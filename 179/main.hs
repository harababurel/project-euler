import Data.Numbers.Primes
import Data.List

divisorCount :: Int -> Int
divisorCount n = product $ map (succ . length) $ group $ primeFactors n

main = print $ sum $ map (pred . length) $ group $ map divisorCount [2..10^7]
