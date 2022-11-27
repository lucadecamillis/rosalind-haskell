rabbit :: Int -> Int -> Int
rabbit n k
    | n == 1 = 1
    | n == 2 = 1
    | otherwise = rabbit (n-1) k + (k * rabbit (n-2) k)

memoizedRabbit :: Int -> Int -> Int
memoizedRabbit n k = map rab [0 ..] !! n
   where
    rab 0 = 0
    rab 1 = 1
    rab n = memoizedRabbit (n-1) k + (k * memoizedRabbit (n-2) k)