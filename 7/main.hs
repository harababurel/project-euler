prime x = null [y | y <- [2..floor (sqrt (fromIntegral x))], mod x y == 0]
main = print (last (take 10001 [x | x <- [2..], prime x]))
