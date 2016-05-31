collatz 1 xs = 1:xs
collatz n xs = n:(collatz n' xs)
    where n'
            | even n    = div n 2
            | otherwise = 3*n + 1

main = print (maximum [(length (collatz n []), n) | n <- [1..10^6]])
