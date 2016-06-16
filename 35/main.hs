import Data.Numbers.Primes (isPrime, primes)
import Data.Char (digitToInt)

rotate :: Int -> Int
rotate n = read $ tail s ++ [head s]
    where s = show n

rotations :: Int -> [Int]
rotations n = rotations' n (length $ show n)
    where rotations' n 1 = [n]
          rotations' n k = n:rotations' (rotate n) (k-1)

isCircular :: Int -> Bool
isCircular n = not $ any (not . isPrime) (rotations n)

candidates = 2:filter (\x -> not $ (any even $ map digitToInt $ show x)) primes
sol = filter isCircular $ takeWhile (<=10^6) candidates

main = do
    sequence $ map print sol
    putStrLn $ "There are " ++ show (length sol) ++ " circular primes."
