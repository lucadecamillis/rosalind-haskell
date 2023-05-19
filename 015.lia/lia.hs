fact n
    | n == 0 = 1
    | otherwise = n * fact (n - 1)

binomialCoeff n k = fact n `div` (fact k * fact (n - k))

choose n 1 = n
choose n k
    | k > n     = -1
    | k > n - k = choose n  (n - k)
    | otherwise = (choose n (k - 1) * (n - k + 1)) `div` k

pmf k n p = fromIntegral (binomialCoeff n k) * (p^k) * ((1-p)^(n-k))