import Data.Ratio
import Data.Char

inverse x  = (denominator x) % (numerator x)

xs = 2:(foldr1 (++) $ map (\x -> 1:x:[1]) [2,4..])

iteration n = iteration' 0
    where iteration' x
            | x == pred n = (xs !! x) % 1
            | otherwise = (xs !! x) % 1 + inverse (iteration' (x+1))

main = print $ sum $ map digitToInt $ show $ numerator $ iteration 100
