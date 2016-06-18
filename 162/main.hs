import Data.Array
import Data.Char (intToDigit, toUpper)
import Numeric (showHex)

dp :: Int -> Int -> Int -> Int -> Integer
dp n z o a  = table ! (n, z, o, a)
    where bnds = ((0,0,0,0), (n,1,1,1))
          table = listArray bnds . map dp' $ range bnds

          dp' :: (Int, Int, Int, Int) -> Integer
          dp' (0, _, _, _) = 0
          dp' (1, 1, 0, 0) = 0 -- can't start with 0
          dp' (1, 0, 1, 0) = 1
          dp' (1, 0, 0, 1) = 1
          dp' (1, 0, 0, 0) = 13
          dp' (1, _, _, _) = 0

          dp' (i, z, o, a) = base + (if z==1 then table ! (pred i, 0, o, a) + table ! (pred i, 1, o, a) else 0) +
                                    (if o==1 then table ! (pred i, z, 0, a) + table ! (pred i, z, 1, a) else 0) +
                                    (if a==1 then table ! (pred i, z, o, 0) + table ! (pred i, z, o, 1) else 0)
              where base = 13 * table ! (pred i, z, o, a) -- by adding anything other than 01a, nothing changes

main = putStrLn $ map toUpper $ showHex sol ""
    where sol = sum $ map (\i -> dp i 1 1 1) [1..16]
