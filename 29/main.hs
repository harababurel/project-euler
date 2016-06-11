import Data.Set
main = print $ length $ Data.Set.fromList [show (a^b) | a <- [2..100], b <- [2..100]]
