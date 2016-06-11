values :: [Integer]
values = [2,5,10,20,50,100,200]

sumWith :: Num a => [a] -> ([a] -> a)
sumWith xs = sum . zipWith (*) xs


sol = length $ filter (<=200) [sumWith values [b,c,d,e,f,g] | b <- [0..100],
                                                             c <- [0..40],
                                                             d <- [0..20],
                                                             e <- [0..10],
                                                             f <- [0..4],
                                                             g <- [0..2]]

main = print $ succ sol
