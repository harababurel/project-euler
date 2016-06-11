lastDigits :: Int -> Int
lastDigits n = foldl (\acc x -> mod (acc * x) (10^10)) 1 $ take n $ repeat n

sol :: Int -> Int
sol n = mod (sum $ map lastDigits [1..n]) (10^10)

main = print $ sol 1000
