import Data.Ratio

invert :: Ratio Integer -> Ratio Integer
invert x = (denominator x) % (numerator x)

iteration :: Int -> Ratio Integer
iteration 1 = 1 + 1%2
iteration n = 1 + invert (1 + iteration (n-1))

digits x = length $ show x

sol = filter (\x -> digits (numerator x) > digits (denominator x)) $ map iteration [1..1000]

main = do
    print $ length sol
