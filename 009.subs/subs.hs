import Data.List

motif :: [Char] -> [Char] -> [Int]
motif s t = [e + 1 | e <- r]
    where r = (t `isPrefixOf`) `findIndices` tails s

main :: IO ()
main = do
    let s = "GATATATGCATATACTT"
    let t = "ATAT"
    print $ motif s t