import Data.List

main = print $ (sort $ permutations [0..9]) !! 999999
