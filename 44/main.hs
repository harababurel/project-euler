isInt x = x == fromIntegral (round x)

pentagonal :: Int -> Bool
pentagonal n = n > 0 && isInt (root n) && mod (1+(floor. root) n) 6 == 0
    where root n = (sqrt . fromIntegral) (1+24*n)

pentagonals = map (\n -> div (n*(3*n-1)) 2) [1..]

sol = [(a, b) | a <- pentagonals,
                b <- take 3000 pentagonals,
                a < b,
                pentagonal (b-a),
                pentagonal (a+b)]

--main = do
--    sequence $ map print sol

main = print $ (snd x) - (fst x)
    where x = head sol
