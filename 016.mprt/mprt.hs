import Common
import Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS as TLS
import Network.HTTP.Request as R
import Data.ByteString.Lazy as L
import Data.ByteString.Char8 as C
import qualified Codec.Binary.UTF8.Generic as U

downloadViaRequest :: [Char] -> IO [Char]
downloadViaRequest url = do
    resp <- get url
    let body = R.responseBody resp
    let hh = R.responseHeaders resp
    let l = C.length body
    print l
    print hh
    let strCnt = U.toString body
    return strCnt

downloadViaHttp :: [Char] -> IO [Char]
downloadViaHttp url = do
    request <- parseRequest ("GET " ++ url)
    manager <- newManager tlsManagerSettings
    response <- httpLbs request manager
    let body = HTTP.responseBody response
    let hh = HTTP.responseHeaders response
    let l = L.length body
    print l
    print hh
    let strCnt = U.toString body
    return strCnt

downloadProtein :: [Char] -> IO [Char]
downloadProtein id = do
    let url = "https://rest.uniprot.org/uniprotkb/" ++ id ++ ".fasta"
    --let url = "https://api.leancloud.cn/1.1/date"
    --let url = "https://cdn.kernel.org/pub/linux/kernel/v6.x/ChangeLog-6.5.2"
    print url
    strCnt1 <- downloadViaHttp url
    strCnt2 <- downloadViaRequest url
    --let content = U.toString body
    --let content = fastaSnd $ parseFastaLines $ responseBody resp
    return strCnt1

main :: IO ()
main = do
    --body <- downloadProtein "B5ZC00"
    body <- downloadProtein "P05067"
    print body