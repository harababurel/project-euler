main = print $ sum [x | x <- [1..10000], x /= d x, d (d x) == x]
    where d n = (map (\n -> sum [x | x <- [1..n-1], mod n x == 0]) [0..]) !! n
