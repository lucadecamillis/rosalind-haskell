fact n
  | n == 0 = 1
  | otherwise = n * fact (n - 1)

binomialCoeff n x = fact n / (fact x * fact (n - x))

prob n x = binomialCoeff n x * (0.25 ** x) * (0.75 ** (n - x))

lia k n = 1 - sum [prob (2**k) i | i <- [0 .. (n-1)]]