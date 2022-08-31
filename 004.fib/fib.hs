rabbit :: Int -> Int -> Int
rabbit n k
    | n == 1 = 1
    | n == 2 = 1
    | otherwise = rabbit (n-1) k + (k * rabbit (n-2) k)