import Data.List

motif :: [Char] -> [Char] -> [Int]
motif s t = (t `isPrefixOf`) `findIndices` tails s

main :: IO ()
main = do
    let s = "GATATATGCATATACTT"
    let t = "ATAT"
    print $ motif s t