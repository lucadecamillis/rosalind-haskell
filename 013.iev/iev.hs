pairProb = [1, 1, 0.5, 0.75, 0.5, 0]

--nrPairs = [1, 0, 0, 1, 0, 1]
nrPairs = [17095, 17760, 19020, 17779, 18384, 19669]

p = sum [ n * p * 2 | (n, p) <- zip nrPairs pairProb ]