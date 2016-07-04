main = print $ dp 50
    where dp i = if i >= 0 then (map dp' [0..]) !! i else 0
          dp' i
            | i <= 1    = 1
            | otherwise = sum $ [dp j | j <- [i-4..i-1]]
