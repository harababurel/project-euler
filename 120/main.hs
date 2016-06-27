raise :: Int -> Int -> Integer
raise a n
    | n == 0 = 1
    | otherwise = (raise a (div n 2))^2 * if odd n then toInteger a else 1

r :: Int -> Int -> Int
r a n = fromIntegral $ mod ((raise (a-1) n) + raise (a+1) n) ((toInteger a)^2)

rmax a = maximum $ map (r a) [1..floor $ 1.5 * fromIntegral a]

main = do
    print $ sum $ map rmax [3..1000]
