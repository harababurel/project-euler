import Data.List
import Data.Set (fromList, toList)

palindrome :: Int -> Bool
palindrome n = show n == (reverse $ show n)

squareSum :: Int -> Int
squareSum n = div (n * (n+1) * (2*n+1)) 6

consecutiveSum :: Int -> Int -> Int
consecutiveSum a b = squareSum b - squareSum (a-1)

sumsWith :: Int -> [Int]
sumsWith b = takeWhile (\a -> consecutiveSum a b < 10^8) [b-1,b-2..1]

sums :: [Int]
sums = [consecutiveSum a b | b <- [1..10^4]
                           , a <- sumsWith b]

main = do
    print $ sum $ fromList $ filter palindrome sums
