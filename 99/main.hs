import Data.List.Split

process :: (Float, Float) -> Float
process (base, exp) = exp * log base

toFloats :: [String] -> [(Float, Float)]
toFloats xs = map (\[a, b] -> (read a, read b)) $ map (\s -> splitOn "," s) xs

sol :: [(Float, Float)] -> [(Float, Int)]
sol xs = zip (map process xs) [1..]

main = do
    input <- getContents
    print $ (snd . maximum . sol . toFloats) (lines input)
