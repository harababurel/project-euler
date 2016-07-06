import Data.Array

digits = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven",
          "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]

tens   = ["", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

letters (a, b, c) = (if a >= 1 then length (digits !! a) + length "hundred" else 0)     -- x "hundred"
                  + (if a >= 1 && b*10+c > 0 then length "and" else 0)                  -- x hundred "and" y
                  + (if b*10+c <= 19 then length (digits !! (b*10+c))                   -- _01 -> _19
                                     else length (tens !! b) + length (digits !! c))    -- _20 -> _99

main = print $ length "onethousand" + sum (map letters $ range ((0,0,0),(9,9,9)))
