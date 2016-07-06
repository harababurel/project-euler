import qualified Data.Set as S
import Data.List

divisors :: Int -> [Int]
divisors n = 1:[x + if x*x /= n then (div n x) else 0 | x <- [2..(floor . sqrt . fromIntegral) n], mod n x == 0]

divisorSum :: Int -> Int
divisorSum n = sum (divisors n)

next = divisorSum

chain :: Int -> [Int]
chain n = chain' n (next n) (S.fromList [n])
    where chain' start n s
            | n >  10^6  = []
            | n == start = S.toList s
            | n <  start = []
            | elem n s   = []
            | otherwise  = chain' start (next n) (S.insert n s)

sol = map (\n -> (length (chain n), n)) $ filter (([] /=). chain) $ [1..10^6]

main = do
    print $ maximum sol
    --sequence $ map print $ sol
    --sequence $ map (print . head) $ group $ scanl1 (\acc x -> if fst x > fst acc then x else acc) sol
