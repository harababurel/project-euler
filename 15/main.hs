fact :: Int -> Integer
fact 0 = 1
fact n = (toInteger n) * fact (n-1)

comb n k = div (fact n) ((fact k) * (fact (n-k)))
main = print (comb 40 20)
