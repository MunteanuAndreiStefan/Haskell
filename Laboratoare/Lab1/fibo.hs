fibo n rez prev
    | n == 0 = rez
    | otherwise = fibo(n-1)(rez + prev) rez