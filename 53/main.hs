fact 0 = 1
fact n = n * fact (n-1)

comb n k = div (fact n) $ (fact (n-k)) * fact k

main = print $ length $ filter (>10^6) [comb n k | n <- [1..100],
                                                   k <- [0..n]]
