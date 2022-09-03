import Common
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = do
    let file = "/home/luca/Desktop/consensus.txt"
    fasta <- C.readFile file
    let lines = parseFastaMultiline fasta
    print lines