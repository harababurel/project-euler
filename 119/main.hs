import Data.Char
import Data.List

main = do
    sequence $ map print $ take 30 $ zip [1..] $ sort $ foldl1 (++) $ map generatedFrom [1..80]
        where generatedFrom n = filter (\x -> x > 9 && digitSum x == n) $ map (\i -> n^i) [1..10]
              digitSum n = sum $ map digitToInt $ show n
