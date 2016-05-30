rev :: (Integral a) => a -> a
rev x = rev' x 0
    where rev' 0 acc = acc
          rev' x acc = rev' (div x 10) (acc * 10 + mod x 10)

main = print (maximum (filter (\x -> x == rev x) [x*y | x <- [100..999], y <- [100..999]]))
