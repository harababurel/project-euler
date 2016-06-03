import Data.List
import Data.IntSet (toList, fromList)

writable = toList $ fromList [x + y | x <- xs, y <- xs, x <= y, x + y <= 28123]
    where xs = filter (\n -> n < sumOfDivisors n) [2..28123]
          sumOfDivisors n = sum [y + div n y | y <- [2..root n], mod n y == 0] - extra n
          root n = floor $ sqrt $ fromIntegral n
          extra n = if (root n)^2 == n then (root n)
                                       else 0
sol  = (div (28123 * 28124) 2) - sum writable
main = print $ sol
