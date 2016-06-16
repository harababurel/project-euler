import Data.Numbers.Primes

truncL :: Int -> [Int]
truncL 0 = []
truncL x = x:truncL (div x 10)

truncR :: Int -> [Int]
truncR n = truncR' n 10
    where truncR' n k
                | mod n k == n = [n]
                | otherwise = (mod n k):truncR' n (k*10)

good :: Int -> Bool
good n = not $ any (not . isPrime) ((truncL n) ++ (truncR n))

sol :: [Int]
sol = sol' [] 4
    where sol' acc i
            | length acc == 11 = acc
            | good x = sol' (x:acc) (succ i)
            | otherwise = sol' acc (i+1)
            where x = primes !! i

main = do
    sequence $ map print sol
    putStrLn $ "Sum = " ++ show (sum sol)
