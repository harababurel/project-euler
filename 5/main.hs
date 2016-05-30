gcd' :: (Integral a, Eq a) => a -> a -> a
gcd' a b = if b == 0 then a
                     else gcd' b (mod a b)

lcm' :: (Integral a, Eq a) => a -> a -> a
lcm' x y = div (x*y) (gcd' x y)

lcmList [x] = x
lcmList (x:y:xs) = lcmList ((lcm' x y):xs)

main = print (lcmList [1..20])
