palindrome :: Integer -> Bool
palindrome n = show n == reverse (show n)

next :: Integer -> Integer
next n = n + read ((reverse . show) n)

iterations :: Int -> Int
iterations n = iterations' 0 (toInteger n)
    where iterations' :: Int -> Integer -> Int
          iterations' acc n
            | palindrome n && acc > 0 = acc
            | acc > 50 = acc
            | otherwise = iterations' (succ acc) (next n)

lychrel :: Int -> Bool
lychrel n = iterations n > 50

sol = filter lychrel [1..10^4]

main = do
    --sequence $ map print sol
    putStrLn $ "There are " ++ (show $ length sol) ++ " Lychrel numbers."
