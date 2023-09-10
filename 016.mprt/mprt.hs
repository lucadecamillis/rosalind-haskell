import Codec.Binary.UTF8.Generic as U
import Common
import Data.ByteString as B
import Https

downloadProtein :: [Char] -> IO [[Char]]
downloadProtein id = do
  httpResult <- getViaRequest $ getFastaUrl id
  --let content = U.toString $ fst httpResult
  let content = fastaSnd $ parseFastaLines $ fst httpResult
  return content

main :: IO ()
main = do
    let id = "B5ZC00"
    body <- downloadProtein id
    print body