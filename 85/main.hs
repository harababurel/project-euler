inside :: Int -> Int -> Int -> Int -> Int
inside n m x y = (n-x+1) * (m-y+1)

total :: Int -> Int -> Int
total n m = sum $ [inside n m x y | x <- [1..n], y <- [1..m]]

sols :: [(Int, Int)]
sols = [(abs (2*10^6 - total n m), n*m) | n <- [1..100], m <- [n..100]]

main = print $ minimum sols
