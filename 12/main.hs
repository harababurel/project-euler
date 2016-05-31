triangle n = div (n * (n+1)) 2
triangles = [triangle n | n <- [1..]]

divs n = 2 * length [x | x <- [1..floor (sqrt (fromIntegral n))], mod n x == 0] - extra n
    where extra n
            | (floor (sqrt (fromIntegral n)))^2 == n = 1
            | otherwise                              = 0

main = print (head (filter (\x -> fst x > 500) [(divs x, x) | x <- triangles]))
