digitSum 0 = 0
digitSum n = (mod n 10) + digitSum (div n 10)

main = print $ maximum [digitSum (a^b) | a <- [1..99],
                                         b <- [1..99]]
