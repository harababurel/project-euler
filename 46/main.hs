import Data.Numbers.Primes

writable :: Int -> Bool
writable n = any isPrime $ takeWhile (>=0) $ map (\x -> n-2*x^2) [1..]

main = print $ head $ filter (\n -> odd n && (not . isPrime) n && (not . writable) n) [2..]
