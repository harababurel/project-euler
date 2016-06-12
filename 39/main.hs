import Data.List (sort)

third :: Int -> Int -> Int
third a b
    | a^2 + b^2 == (candidate a b)^2 = candidate a b
    | otherwise = 0
    where candidate a b = floor $ sqrt $ fromIntegral (a^2 + b^2)


perimeters :: [Int]
perimeters = filter (<=1000) [a + b + third a b | a <- [1..500],
                                                 b <- [1..500],
                                                 third a b /= 0]


compress :: (Int, Int) -> [Int] -> [(Int, Int)]
compress acc [] = [acc]
compress acc (x:xs)
    | x == snd acc = compress (1 + fst acc, snd acc) xs
    | otherwise = acc:compress (1, x) xs

sol :: [Int] -> (Int, Int)
sol xs = maximum $ compress (1, head xs) (tail xs)

main = print $ sol $ sort perimeters
