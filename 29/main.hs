import Data.Set (fromList, toList)
main = print $ length $ toList $ fromList [show (a^b) | a <- [2..100], b <- [2..100]]
