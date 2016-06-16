p = 10^10 :: Integer

raise :: Integer -> Integer -> Integer
raise n 0 = 1
raise n k
    | even k = mod (half^2) p
    | otherwise =  mod (n * half^2) p
    where half = raise n (div k 2)

main = print $ mod (succ $ 28433 * raise 2 7830457) p
