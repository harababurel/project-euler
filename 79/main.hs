import Data.List (sort, permutations)

matches :: String -> String -> Bool
matches attempt passcode = matches' attempt passcode
    where matches' [] _ = True
          matches' _ [] = False
          matches' (x:xs) (y:ys) = if x == y then matches' xs ys
                                             else matches' (x:xs) ys

good n attempts = not $ any (\x -> matches x n == False) attempts

main = do
    input <- getContents
    putStrLn $ head $ filter (\x -> good x (words input)) $ permutations "12367890"
