good :: Int -> Bool
good n = let s = show n in map (s !!) [0,2..length $ s] == "1234567890"

main = print $ head $ filter (\x -> good (x^2)) $ reverse [lo,lo+10..hi]
    where lo = 1010101010 :: Int
          hi = 1389026624 :: Int
