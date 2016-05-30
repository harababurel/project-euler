fibo 1 = 1
fibo 2 = 2
fibo n = fibo (n-1) + fibo (n-2)

sol = sum (filter even (takeWhile (<= 4 * 10^6)  [fibo x | x <- [1..]]))
main = print sol
