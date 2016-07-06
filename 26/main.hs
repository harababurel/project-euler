period n = period' (decimals n) 1
    where decimals n = drop 10 $ show $ div (10^2000) n
          period' n i
            | 2*i       >  length          n = 0
            | take i n  == take i (drop i n) = i
            | otherwise                      = period' n (i+1)

main = print $ maximum $ zip (map period [1..999]) [1..]
