import Data.List  (sort)
import Data.Array (range)

squareDist :: Int -> Int -> Int -> Int -> Int
squareDist x y x' y' = (x-x')^2 + (y-y')^2

right :: Int -> Int -> Int -> Int -> Bool
right x y x' y' = d!!0 + d!!1 == d!!2
    where d = sort $ [squareDist 0 0 x  y
                     ,squareDist 0 0 x' y'
                     ,squareDist x y x' y']

sols :: Int -> [(Int, Int, Int, Int)]
sols n = [(x, y, x', y') | (x, y, x', y') <- range ((0,0,0,0),(n,n,n,n))
                         , not ((x, y) == (x', y'))
                         , not ((0, 0) == (x', y'))
                         , not ((x, y) == (0, 0))
                         , (x, y) < (x', y')
                         , right x y x' y']

main = do
    print $ length $ sols 50
