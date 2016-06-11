fibs :: [Integer]
fibs = 1:1:zipWith (+) fibs (tail fibs)

main = print $ succ $ length $ takeWhile (\x -> length x < 1000) $ map show fibs
