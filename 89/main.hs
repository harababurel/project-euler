import Text.Numeral.Roman
import Data.Maybe (fromJust)
import Data.String.Utils

correct s = replace "IV" "IIII" $
            replace "IX" "VIIII" $
            replace "XL" "XXXX" $
            replace "XC" "LXXXX" $
            replace "CD" "CCCC" $
            replace "CM" "DCCCC" s

value s = sum $ map (fromJust . flip lookup (zip "IVXLCDM" [1,5,10,50,100,500,1000])) $ correct s

main = do
    input <- getContents
    print $ sum $ map (\x -> length x - length (toRoman (value x) :: String)) $  words input
