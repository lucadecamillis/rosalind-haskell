import Common
import Network.HTTP.Request
import Data.ByteString as B

downloadProtein :: [Char] -> IO [[Char]]
downloadProtein id = do
    let url = "https://rest.uniprot.org/uniprotkb/" ++ id ++ ".fasta"
    resp <- get url
    let content = fastaSnd $ parseFastaLines $ responseBody resp
    return content

main :: IO ()
main = do
    body <- downloadProtein "B5ZC00"
    print body