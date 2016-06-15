removeTrailing :: Int -> Int
removeTrailing 0 = 0
removeTrailing n
    | mod n 10 == 0 = removeTrailing $ div n 10
    | otherwise = n

f :: Int -> Int -> Int
f acc 0 = acc
f acc n = f (mod (removeTrailing $ n * acc) (10^9)) (n-1)

main = print $ f 1 (10^5) `mod` (10^9)
