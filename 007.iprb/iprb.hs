iprb :: Int -> Int -> Int -> Double
iprb k m n =
    (xy k m tot * km) * 2.0 +
    (xy k n tot * kn) * 2.0 +
    (xy m n tot * mn) * 2.0 +
    xx k tot * kk +
    xx m tot * mm
    where
        km = 1.0
        kn = 1.0
        mn = 0.5
        kk = 1.0
        mm = 0.75
        nn = 0.0
        tot = k + m + n
        xx nx tot = fromIntegral (nx * (nx - 1)) / fromIntegral (tot * (tot - 1))
        xy nx ny tot = fromIntegral (nx * ny) / fromIntegral (tot * (tot - 1))