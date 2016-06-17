main = print $ sum $ takeWhile (>0) $ map (\e -> length $ takeWhile (\i -> e^2-i^2<=10^6) [e-2,e-4..1]) ([3..]::[Int])
