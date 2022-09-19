import Common

computeAdjacencyList :: Int -> [([Char], [Char])] -> [([Char], [Char])]
computeAdjacencyList k l = [ (fst x, fst y) | x <- l, y <-l, areAdjacent k (snd x) (snd y) ]
    where
        areAdjacent k x y = x /= y && lengthCheck x y && suffixEqPrefix k x y
        lengthCheck x y = length x >= k && length y >= k
        suffixEqPrefix k x y = drop (length x - k) x == take k y

main :: IO ()
main = do
    let input = "/home/luca/Desktop/grph.txt"
    let output = "/home/luca/Desktop/grph_res.txt"
    lines <- fastaTuples <$> readFastaLines input
    let adjacencyList = computeAdjacencyList 3 lines
    writeLines output [ unwords [ e1, e2 ] | (e1, e2) <- adjacencyList ]
    print adjacencyList