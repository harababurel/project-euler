import Data.Char
import Data.Matrix
import qualified Data.Matrix as Matrix

fibos :: [Int]
fibos = 1:1:zipWith (\a b -> mod (a+b) (10^9)) fibos (tail fibos)

square m = multStd m m

raise m n
    | n == 1 = m
    | even n = square $ raise m (div n 2)
    | otherwise = multStd m $ square $ raise m (div n 2)

z = fromLists [[0,1],[1,1]]
nthFibo n = (multStd (fromLists [[0, 1]]) $ raise z n) ! (1,1)

pandigital :: String -> Bool
pandigital s
    | length s /= 9 = False
    | sum (map digitToInt s) /= 45 = False
    | any (==False) [elem i s | i <- ['1'..'9']] = False
    | otherwise = True

good (i, x)
    | not $ pandigital $ show x = False
    | not $ pandigital $ take 9 $ large i = False
    | otherwise = True
    where large i = show $ nthFibo i

main = do
    print $ fst $ head $ filter good $ zip [1..] fibos
