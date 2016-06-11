import Data.Char

sumOfPowers n = sum $ map (^5) $ map digitToInt $ show n

sol = filter (\n -> n == sumOfPowers n) [2..354294]
main = print $ sum sol

-- 354294 = sumOfPowers 999999
-- which is enough because from this point onwards
-- the sum can't possibly be greater than its
-- generator value.
