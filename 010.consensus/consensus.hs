import Common
import Data.List
import Data.Text.Encoding
import qualified Data.ByteString.Char8 as C

parseLines :: [(C.ByteString, C.ByteString)] -> [[Char]]
parseLines l = [ [ x | x <- y ] | y <- r ]
    where r = [ C.unpack (snd e) | e <- l ]

main :: IO ()
main = do
    let file = "/home/luca/Desktop/consensus.txt"
    fasta <- C.readFile file
    let lines = parseFastaMultiline fasta
    let matrix = parseLines lines
    print matrix