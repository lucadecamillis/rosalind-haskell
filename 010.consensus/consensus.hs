import Common
import Data.List
import Data.Text.Encoding
import qualified Data.ByteString.Char8 as C

parseLines :: [(C.ByteString, C.ByteString)] -> [[Char]]
parseLines l = [ [ x | x <- y ] | y <- r ]
    where r = [ C.unpack (snd e) | e <- l ]

computeProfile :: [[Char]] -> [(Char, [Int])]
computeProfile m = [ (s, countOccurence s m) | s <- ['A', 'C', 'G', 'T'] ]

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
    print profile