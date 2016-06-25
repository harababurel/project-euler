import Data.Ratio

main = print $ maximum $ filter (<3%7) [(bestNumeratorFor b) % b | b <- [1..10^6]]
    where bestNumeratorFor x = floor $ 3 * (fromIntegral x) / 7
