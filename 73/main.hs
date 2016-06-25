import Data.Ratio

sol :: [Ratio Int]
sol = [a%b | b <- [1..12000],
             a <- [div b 3..div b 2],
             2*a < b && b < 3*a,
             gcd a b == 1]

main = print $ length sol
