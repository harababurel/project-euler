import Data.Numbers.Primes
import Data.List

rad :: Int -> Int
rad n = product $ map head $ group $ primeFactors n

main = do
    print $ snd $ (sort $ map (\i -> (rad i, i)) [1..100000]) !! 9999
