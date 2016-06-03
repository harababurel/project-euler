sumOfDivisors :: Int -> Int
sumOfDivisors x = xs !! x
    where xs = map (\n -> 1 - (extra n) + sum [y + div n y | y <- [2..root n], mod n y == 0]) [0..]
          root :: Int -> Int
          root n = floor $ sqrt $ fromIntegral n
          extra n = if (root n)^2 == n then (root n)
                                       else 0
abundant :: Int -> Bool
abundant n = (sumOfDivisors n) > n

notWritable :: Int -> Bool
notWritable n = null [1 | x <- [1..div n 2], abundant x, abundant (n-x)]

sol = filter notWritable [1..28123]

main = print $ show sol ++ " " ++ show (sum sol)
