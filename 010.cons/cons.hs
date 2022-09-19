import Common
import Data.List
import Data.Ord

alphabet = ['A', 'C', 'G', 'T']

computeProfile :: [[Char]] -> [[Int]]
computeProfile m = [ countOccurence s m | s <- alphabet ]

countOccurence :: Char -> [[Char]] -> [Int]
countOccurence s m = [ aggregate row | row <- m ]
    where aggregate = length . filter (== s)

computeConsensus :: [[Int]] -> [Int]
computeConsensus p = [ snd (getMax e) | e <- p ]
    where getMax a = maximumBy (comparing fst) (zip a [0..])

printProfile :: [[Int]] -> [String]
printProfile p = [ printLine e | e <- nameProfile p ]
    where
        nameProfile = zip alphabet
        printLine (name, items) = name : ": " ++ unwords [ show e | e <- items ]

main :: IO ()
main = do
    let input = "/home/luca/Desktop/consensus.txt"
    let output = "/home/luca/Desktop/consensus_res.txt"
    lines <- readFastaLines input
    let profile = computeProfile $ transpose (fastaSnd lines)
    let consensus = [ alphabet !! e | e <- computeConsensus $ transpose profile ]
    let result = consensus : printProfile profile
    writeLines output result
    print result