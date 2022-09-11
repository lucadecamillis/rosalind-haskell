import Data.Binary

rabbitsSoe :: Int -> Int -> Word64
rabbitsSoe n m
  | n == 1 = 1
  | n == 2 = 1
  | n < m = current n m
  | n == m = current n m - 1
  | otherwise = current n m - current (n - m) m
  where
    current n m = rabbitsSoe (n -1) m + rabbitsSoe (n -2) m

rabbits :: Int -> Integer
rabbits n = innerRabbits (n-1)

innerRabbits :: Int -> Integer
innerRabbits = (seq !!)
  where
    m = 20
    seq = 1:1:[sut i m | i <- [2 ..]]
    sut i m
      | i <  m = current i
      | i == m = current i - 1
      | otherwise = current i - innerRabbits (i - m - 1)
    current i = innerRabbits (i -1) + innerRabbits (i -2)