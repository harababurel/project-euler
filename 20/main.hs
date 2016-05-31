import Data.Char

fact :: Int -> Integer
fact 0 = 1
fact n = (toInteger n) * fact (n-1)

digitSum :: Integer -> Int
digitSum n = sum (map digitToInt (show n))

main = print (digitSum (fact 100))
