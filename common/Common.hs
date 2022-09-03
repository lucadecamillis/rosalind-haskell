module Common where

import qualified Data.ByteString.Char8 as C

-- https://github.com/kerkomen/rosalind-haskell/blob/master/stronghold/GC.hs
parseFastaMultiline :: C.ByteString -> [(C.ByteString, C.ByteString)]
parseFastaMultiline f = zip (map (C.takeWhile (/='\n')) xs) (map (C.filter (/='\n') . C.dropWhile (/='\n')) xs)
    where xs = filter (\x -> C.length x > 0) . C.split '>' $ f