import Common
import Data.List
import Data.Text.Encoding
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

main :: IO ()
main = do
    let file = "/home/luca/Desktop/consensus.txt"
    fasta <- C.readFile file
    let lines = parseFastaMultiline fasta
    let matrix = parseLines lines
    let profile = computeProfile $ transpose matrix
    printMatrix profile