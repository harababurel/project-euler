third :: (Integral a) => a -> a -> a
third a b = if a^2 + b^2 == c^2 then c
                                else 0
    where c = floor (sqrt (fromIntegral (a^2 + b^2)))

main = print (head [product [a, b, third a b] | a <- [1..], b <- [a..400], third a b /= 0, a + b + third a b == 1000])
