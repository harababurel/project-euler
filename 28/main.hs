deltas = foldr1 (++) $ map (replicate 4) [2,4..]
xs = scanl (+) 1 deltas

main = print $ sum $ take (2*1001-1) xs
