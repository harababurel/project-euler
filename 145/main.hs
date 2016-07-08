-- Note: show and read are slow. Use custom functions instead.
-- Reduced execution time from 31s to 2.8s this way.

digits :: Int -> [Int]
digits n
    | n < 10 = [n]
    | otherwise = (mod n 10):digits (div n 10)

rev = (foldl1 (\acc x -> acc*10+x)) . digits

good :: Int -> Bool
good n = (mod n 10 /= 0) && (not $ any (\x -> mod x 2 == 0) $ digits (n + rev n))

brute :: Int -> Int
brute n = length $ filter good [10^(n-1)..(10^n)-1]

-- (1)
-- When adding a pair of digits which are not of the form (first, last),
-- they can range between 0 and 9, provided they hold som properties:
--     - their sum is odd
--     - they don't overflow
-- There are 30 such pairs.
insideSize 2 = 30
insideSize n
    | even n = 30 * insideSize (n-2)
    | odd n  = 0

-- (2)
-- When adding a pair of digits of the form (first, last),
-- they cannot be zero. Both properties from (1) must hold.
ofSize :: Int -> Int
ofSize 1 = 0
ofSize 2 = 20
ofSize n
    | even n = 20 * insideSize (n-2)
    | odd n  = 0

-- (3)
-- For some reason, there are no solutions with 5 or 9 digits,
-- but there are some with 3 or 7. These can be counted manually.
sol = sum (map ofSize [2..9])
    + sum (map brute [1,3,7])

main = print sol
