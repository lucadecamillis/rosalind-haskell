import Https
import Common
import Data.ByteString as B

getDownloadPath :: [Char] -> [Char] -> [Char]
getDownloadPath id tt = "/home/luca/Desktop/" ++ id ++ ".HS." ++ tt ++ ".fasta"

diagHttps :: [Char] -> [Char] -> ([Char] -> IO (ByteString, Int)) -> IO ()
diagHttps id tt f = do
  httpResult <- f $ getFastaUrl id
  print ("Http download (" ++ tt ++ "), nr bytes " ++ show (snd httpResult) ++ " writing to file")
  let filePath = getDownloadPath id tt
  B.writeFile filePath (fst httpResult)

testHttpClients :: IO ()
testHttpClients = do
  --let id = "P05067"
  let id = "B5ZC00"
  diagHttps id "snoyberg" getViaSnoyberg
  diagHttps id "request" getViaRequest
  diagHttps id "aesiniath" getViaAesiniath
  diagHttps id "conduit" getViaConduit