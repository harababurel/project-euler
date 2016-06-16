generate :: Int -> String
generate x = generate' "" x 1
    where generate' acc x i
                | length acc >= 9 = acc
                | otherwise = generate' (acc ++ (show $ x*i)) x (i+1)

pandigital :: String -> Bool
pandigital n
    | length n /= 9 = False
    | any (\d -> not $ elem d n) ['1'..'9'] = False
    | otherwise = True

sol :: [Int]
sol = map read $ filter pandigital $ map generate [1..10000]

main = do
    sequence $ map print sol
    putStrLn $ "sol = " ++ (show $ maximum sol)
