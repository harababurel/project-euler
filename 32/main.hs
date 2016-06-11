import Data.Char
import Data.Set (fromList, toList)

count :: Eq a => a -> [a] -> Int
count x xs = length $ filter (==x) xs

pandigital :: Int -> Bool
pandigital n = length (show n) == 9 && (null $ filter (/= 1) $ map (\x -> count x listOfDigits) [1..9])
    where listOfDigits = map digitToInt $ show n

stringToInt :: [Char] -> Int
stringToInt xs = foldl (\acc x -> acc * 10 + x) 0 $ map Data.Char.digitToInt xs

sol :: [Int]
sol = toList $ fromList [a*b | a <- [0..2000],
                               b <- [0..2000],
                               a <= b,
                               pandigital $ stringToInt (show a ++ show b ++ show (a*b))]

main = print $ sum sol
