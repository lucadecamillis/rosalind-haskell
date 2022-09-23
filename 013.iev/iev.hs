pairProb = [1, 1, 1, 0.75, 0.5, 0]

nrPairs = [1, 0, 0, 1, 0, 1]

p = sum [ n * p * 2 | (n, p) <- zip nrPairs pairProb ]