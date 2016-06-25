import qualified Data.Set as Set
import Data.Char

{-
fact :: Int -> Int
fact n = fact' n 1
    where fact' 0 acc = acc
          fact' n acc = fact' (n-1) (acc*n)
-}

fact :: Int -> Int
fact n = [1,1,2,6,24,120,720,5040,40320,362880] !! n

{-
next :: Int -> Int
next n = next' n 0
    where next' 0 acc = acc
          next' n acc = next' (div n 10) (acc + fact (mod n 10))
-}

next :: Int -> Int
next n = sum $ map fact $ map digitToInt $ show n

goodChain :: Int -> Bool
goodChain n = goodChain' n Set.empty
    where goodChain' n acc
            | Set.member n acc = (Set.size acc) == 60
            | Set.size acc > 60 = False
            | otherwise = goodChain' (next n) (Set.insert n acc)

main = print $ length $ filter (==True) $ map goodChain [1..10^6]
