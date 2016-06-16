import Data.Char

next :: Int -> Int
next n = sum $ map (^2) $ map digitToInt $ show n

good :: Int -> Bool
good n
    | n == 1 = False
    | n == 89 = True
    | otherwise = good (next n)

main = print $ length $ filter good [1..10^7]

{-
chain :: Int -> [Int]
chain n = chain' n [] where
    chain' 1 acc = 1:acc
    chain' 89 acc = 89:acc
    chain' n acc = chain' (next n) (n:acc)

sol = map last $ filter (\xs -> head xs == 89) $ map chain [1..10^5]
-}
