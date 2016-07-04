import Data.Array

conf :: Int -> Int -> Int
conf n tile = (colors ! n + blacks ! n) - 1
    where blacks = (listArray (0,n) . map black) [0..n]
          black n
            | n <= 2 = 1
            | otherwise = blacks ! (n-1) + color (n-1)

          colors = (listArray (0,n) . map color) [0..n]
          color n
            | n < tile = 0
            | n == tile = 1
            | otherwise = blacks ! (n-tile) + colors ! (n-tile)

n = 50
main = print $ conf n 2 + conf n 3 + conf n 4
