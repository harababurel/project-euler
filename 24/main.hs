import Data.List

main = putStrLn $ (sort $ permutations ['0'..'9']) !! 999999
