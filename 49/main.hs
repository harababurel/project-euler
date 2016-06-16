import Data.List (permutations)
import Data.Numbers.Primes (isPrime)

diffs :: Int -> [Int]
diffs n = filter (>0) $ map (subtract n) $ map read $ permutations $ show n

sol :: Int -> Maybe Int
sol n
    | null xs = Nothing
    | otherwise = Just (head xs)
    where xs = map read [concat $ map show [n, n+x, n+x+x] | x <- filter (\x -> (2*x) `elem` diffs n) $ diffs n,
                                                             isPrime (n+x),
                                                             isPrime (n+x+x)]

main = do
    sequence $ map print $ map (\(Just n) -> n) $ filter (/= Nothing) $ map sol $ filter isPrime [1000..9999]
