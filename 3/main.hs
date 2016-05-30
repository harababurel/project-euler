n :: Int
n = 600851475143

prime :: (Integral a, Eq a, Enum a) => a -> Bool -- not entirely necessary
prime x = (length [y | y <- [2..floor (sqrt (fromIntegral x))], x `mod` y == 0]) == 0

largestPrimeDiv n = maximum [x | x <- [2..floor (sqrt (fromIntegral n))], n `mod` x == 0, prime x]

main = print (largestPrimeDiv n)
