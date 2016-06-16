isInt x = x == (fromIntegral . round) x

hexagonals :: [Int]
hexagonals = map (\n -> n*(2*n-1)) [1..]

-- k = (1 +- sqrt(1+24n)) / 6
isPentagonal :: Int -> Bool
isPentagonal n
    | not $ isInt (root n) = False
    | mod (1 + (floor . root) n) 6 /= 0 = False
    | otherwise = True
    where root n = sqrt (1 + 24 * fromIntegral n)

-- k = (-1 +- sqrt(1+8n)) / 2
isTriangle :: Int -> Bool
isTriangle n
    | not $ isInt (root n) = False
    | mod (-1 + (floor . root) n) 2 /= 0 = False
    | otherwise = True
    where root n = sqrt (1 + 8 * fromIntegral n)

sol = filter (\n -> isTriangle n && isPentagonal n) hexagonals
main = print $ sol !! 2
