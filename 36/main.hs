import Numeric (showIntAtBase)
import Data.Char (intToDigit)

bin x = showIntAtBase 2 intToDigit (read x) ""
pal x = x == reverse x

sol :: [Int]
sol = map read $ filter (\x -> pal x && (pal . bin) x) $ map show [1..10^6]

main = print $ sum sol
