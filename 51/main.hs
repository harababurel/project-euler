import Data.Numbers.Primes
import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Data.List

bin n = showIntAtBase 2 intToDigit n ""

padWithZeros n x = (replicate (length (show n) - length x) '0') ++ x
masks n = map (padWithZeros n) $ takeWhile (\x -> length x <= length (show n)) $ map bin [1..]

-- replace bits indicated by mask with digit
-- only if the masked digits are the same (that is,
-- n is a member of its own family)
derivate n mask digit
    | length (group [(show n) !! i | i <- [0..pred $ length mask], mask !! i == '1']) /= 1 = 0
    | otherwise = read [if mask !! i == '1' then digit
                                            else (show n) !! i | i <- [0..pred $ length mask]] :: Int

derivatives n mask = filter isPrime $ filter (>=n) [derivate n mask digit | digit <- ['0'..'9']]
families n = [derivatives n mask | mask <- masks n]
bestFamily n = snd $ maximum $ map (\family -> (length family, family)) (families n)

main = print $ head $ filter (\x -> length (bestFamily x) >= 8) primes
