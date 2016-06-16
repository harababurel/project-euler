import Data.Ratio

{- xy / yz = x / z
 - xy * z  = yz * x
 - (10x + y) * z = (10y + z) * x
 - 10xz + yz = 10xy + xz
 - 10x(z-y) = z(x-y)
 -}

sols = [(x*10+y) % (y*10+z) | x <- [1..9],
                              y <- [1..9],
                              z <- [1..9],
                              10*x*(z-y) == z*(x-y),
                              x /= z]

main = print $ denominator $ product sols
