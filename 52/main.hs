frequencies :: Int -> [Int]
frequencies n = map (\x -> length $ filter (==x) (show n)) ['0'..'9']

sameDigits :: Int -> Int -> Bool
sameDigits a b = frequencies a == frequencies b

allTheSame :: [Int] -> Bool
allTheSame (x:xs) = not $ any (\y -> not $ sameDigits x y) xs

main = print $ head $ filter (\x -> allTheSame $ map (*x) [2..6]) [1..]
