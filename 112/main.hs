takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x:if p x then takeWhileInclusive p xs
                                       else []

increasing :: Int -> Bool
increasing n
    | n < 10 = True
    | div (mod n 100) 10 > mod n 10 = False
    | otherwise = increasing (div n 10)

decreasing :: Int -> Bool
decreasing n
    | n < 10 = True
    | div (mod n 100) 10 < mod n 10 = False
    | otherwise = decreasing (div n 10)

bouncy :: Int -> Int
bouncy n = if (not $ increasing n) && (not $ decreasing n) then 1 else 0

xs = map (\i -> (i, bouncy i)) [1..]

sol = map (\x -> (fst x, (fromIntegral $ snd x) / (fromIntegral $ fst x))) $ scanl1 (\acc x -> (fst x, snd acc + snd x)) $ xs

main = print $ (succ . fst . last) $ takeWhile (\x -> snd x < 0.99) sol
