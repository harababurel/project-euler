import Data.Numbers.Primes
import Data.Set (fromList, toList)

sol m = [a^2 + b^3 + c^4 | a <- takeWhile (\x -> x^2 < m) primes
                         , b <- takeWhile (\x -> a^2 + x^3 < m) primes
                         , c <- takeWhile (\x -> a^2 + b^3 + x^4 < m) primes]

main = print $ length $ fromList $ sol 50000000
