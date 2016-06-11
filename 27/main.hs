prime n
    | n <= 1    = False
    | otherwise = null $ filter divisible $ takeWhile (\y -> y*y <= n) [2..]
    where divisible y = mod n y == 0

chain a b = takeWhile prime $ map (\n -> n^2 + a*n + b) [0..]
longestChain n = maximum [(length $ chain a b, a, b) | a <- [-n..n], b <- [-n..n]]

sol (_, a, b) = a * b

main = print $ sol $ longestChain 999
