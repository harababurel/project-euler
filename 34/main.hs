import Data.Char (digitToInt)

fact :: Int -> Int
fact n = product [1..n]

curious :: Int -> Bool
curious n = n == (sum $ map fact $ map digitToInt $ show n)

sol = filter curious [3..10^5]

main = do
    sequence $ map print $ sol
    putStrLn $ "Sum = " ++ show (sum sol)
