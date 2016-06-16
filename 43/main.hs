import Data.List (permutations)
import Data.Char (digitToInt)

mininumber :: String -> Int -> Int
mininumber n i = read $ (n !! (i-1)):(n !! i):[n !! (i+1)]

sol :: [Int]
sol = [read n | n <- permutations ['0'..'9'],
                mod (digitToInt $ n !! 3) 2 == 0,
                mod (mininumber n 8) 17 == 0,
                mod (mininumber n 7) 13 == 0,
                mod (mininumber n 6) 11 == 0,
                mod (mininumber n 5) 7 == 0,
                mod (mininumber n 4) 5 == 0,
                mod (mininumber n 3) 3 == 0]

-- ^ conditions apply in the order they filter best:
-- first one is really easy to compute, while checking
-- (% 17 == 0) filters a lot more values than (% 3 == 0)

main = do
    sequence $ map print sol
    putStrLn $ "Sum is " ++ (show $ sum sol)
