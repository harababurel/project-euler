import Data.Maybe (fromJust)

takeWhileInclusive p [] = []
takeWhileInclusive p (x:xs)
    | p x = x:(takeWhileInclusive p xs)
    | otherwise = [x]

blue :: Integer -> Maybe Integer
blue n
    | (blue' n) * (pred $ blue' n) * 2 == n * (n-1) = Just (blue' n)
    | otherwise = Nothing
    where blue' n = ceiling $ sqrt $ (fromIntegral n)*(fromIntegral n-1)/2

sol = map (\x -> (fst x, fromJust $ snd x)) $ filter (\x -> snd x /= Nothing) $ zip [start..] $ map blue [start..]
    where start = 1

-- http://oeis.org/A046090
a 0 = 1
a 1 = 4
a n = 6 * a (n-1) - a (n-2) - 2

main = do
    sequence $ map print $ takeWhileInclusive (\x -> fst x < 10^12) $ map (\x -> (x, blue x)) $ map a [1..]
