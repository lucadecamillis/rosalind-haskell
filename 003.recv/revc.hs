complement :: Char -> Char
complement c
    | c == 'A' = 'T'
    | c == 'T' = 'A'
    | c == 'C' = 'G'
    | c == 'G' = 'C'
    | otherwise = c

reverseComplement :: [Char] -> [Char]
reverseComplement e = [ c | x <- reverse e, let c = complement x ]