module Https where

import Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS as TLS
import Network.HTTP.Request as R
import Data.ByteString.Lazy as L
import Data.ByteString.Char8 as C
import Network.Http.Client as AES
import Data.ByteString as B
import Network.HTTP.Simple as S
import qualified Network.HTTP.Types as H

getViaAesiniath ::[Char] -> IO (B.ByteString, Int)
getViaAesiniath url = do
    resp <- AES.get (C.pack url) AES.simpleHandler
    let body = resp
    let l = C.length body
    return (body, l)

getViaRequest :: [Char] -> IO (B.ByteString, Int)
getViaRequest url = do
    resp <- R.get url
    let body = R.responseBody resp
    let hh = R.responseHeaders resp
    let l = C.length body
    return (body, l)

getViaSnoyberg :: [Char] -> IO (B.ByteString, Int)
getViaSnoyberg url = do
    request <- HTTP.parseRequest ("GET " ++ url)
    manager <- newManager tlsManagerSettings
    response <- HTTP.httpLbs request manager
    let body = HTTP.responseBody response
    let hh = HTTP.responseHeaders response
    let l = L.length body
    return (L.toStrict body, fromIntegral l)

getViaConduit :: [Char] -> IO (B.ByteString, Int)
getViaConduit url = do
    request <- S.parseRequest url
    let requestWithHeaders = S.addRequestHeader H.hUserAgent (C.pack "YourAppName/1.0") request
    response <- S.httpBS request
    let body = getResponseBody response
    let l = C.length body
    return (body, l)