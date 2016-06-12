import Data.Char (digitToInt)

champernowne = foldr (\x acc -> x++acc) [] $ map show [1..]
d n = digitToInt $ champernowne !! (n-1)

main = print $ product $ map d $ map (\n -> 10^n) [0..6]
