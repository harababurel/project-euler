good :: (Int, Int) -> Bool
good (a, n) = n == length (show ((toInteger a)^n))

sol = filter good [(a, n) | a <- [1..10],
                            n <- [1..50]]

main = do
    sequence $ map print sol
    putStrLn $ "There are " ++ show (length sol) ++ " numbers."
