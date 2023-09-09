import Common
import Https
import Data.ByteString as B
import Codec.Binary.UTF8.Generic as U

downloadProtein :: [Char] -> IO [Char]
downloadProtein id = do
    httpResult <- getViaRequest $ getFastaUrl id
    let content = U.toString $ fst httpResult
    --let content = fastaSnd $ parseFastaLines $ responseBody resp
    return content

getFastaUrl :: [Char] -> [Char]
getFastaUrl id = "https://rest.uniprot.org/uniprotkb/" ++ id ++ ".fasta"

getDownloadPath :: [Char] -> [Char] -> [Char]
getDownloadPath id tt = "/home/luca/Desktop/" ++ id ++ ".HS." ++ tt ++ ".fasta"

diagHttps :: [Char] -> [Char] -> ([Char] -> IO (ByteString, Int)) -> IO ()
diagHttps id tt f = do
    httpResult <- f $ getFastaUrl id
    print ("Http download (" ++ tt ++ "), nr bytes " ++ show (snd httpResult) ++ " writing to file")
    let filePath = getDownloadPath id tt
    B.writeFile filePath (fst httpResult)

main :: IO ()
main = do
    --let id = "P05067"
    let id = "B5ZC00"
    diagHttps id "snoyberg" getViaSnoyberg
    diagHttps id "request" getViaRequest
    diagHttps id "aesiniath" getViaAesiniath
    --body <- downloadProtein id
    --print body