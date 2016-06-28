import Data.List
import Data.Array
-- dp i j = number of configurations of length i that and with j and are non-(decreasing|increasing)

increasing n
    | n <= 9 = True
    | div (mod n 100) 10 > mod n 10 = False
    | otherwise = increasing (div n 10)

decreasing n
    | n <= 9 = True
    | div (mod n 100) 10 < mod n 10 = False
    | otherwise = decreasing (div n 10)

bouncy n = (not $ increasing n) && (not $ decreasing n)

-- ^helper functions, not used in solution

memoize f bnds = listArray bnds . map f $ range bnds

increasingOfLength :: Int -> Int
increasingOfLength n = sum [dp ! (n,j) | j <- [0..9]]
    where dp = memoize dp' ((0,0), (n,9))
          dp' (1,0) = 0
          dp' (1,_) = 1
          dp' (i,j) = sum [dp ! (i-1,j') | j' <- [1..j]]

decreasingOfLength :: Int -> Int
decreasingOfLength n = sum [dp ! (n,j) | j <- [0..9]]
    where dp = memoize dp' ((0,0),(n,9))
          dp' (1,0) = 0
          dp' (1,_) = 1
          dp' (i,j) = sum [dp ! (i-1,j') | j' <- [j..9]]

notBouncyOfLength :: Int -> Int
notBouncyOfLength n = increasingOfLength n + decreasingOfLength n - 9
--           configurations with identical digits are counted twice ^

main = do
    print $ sum $ map notBouncyOfLength [1..100]
