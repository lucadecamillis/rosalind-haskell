import Common
import Data.List
import Data.Ord
import qualified Data.ByteString.Char8 as C

alphabet = ['A', 'C', 'G', 'T']

parseLines :: [(C.ByteString, C.ByteString)] -> [[Char]]
parseLines l = [ [ x | x <- y ] | y <- r ]
    where r = [ C.unpack (snd e) | e <- l ]

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
    let input = "/home/luca/Desktop/rosalind_cons.txt"
    let output = "/home/luca/Desktop/rosalind_cons_res.txt"
    fasta <- C.readFile input
    let lines = parseFastaMultiline fasta
    let matrix = parseLines lines
    let profile = computeProfile $ transpose matrix
    let consensus = [ alphabet !! e | e <- computeConsensus $ transpose profile ]
    let result = consensus : printProfile profile
    C.writeFile output (C.pack (unlines result))
    print result