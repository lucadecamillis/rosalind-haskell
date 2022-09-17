module Common where

import qualified Data.ByteString.Char8 as C
import qualified Data.Bifunctor
import Text.Read (Lexeme(Char))

-- |Read 'fasta' file data into a 2 column data structure.
-- The second column contains the actual data
readFastaMultiline :: FilePath -> IO[(C.ByteString, C.ByteString)]
readFastaMultiline path = parseFastaMultiline <$> C.readFile path

-- |Write the given array of string into the given output file
writeLines :: FilePath -> [[Char]] -> IO()
writeLines path lines = C.writeFile path (C.pack (unlines lines))

-- |Parse fasta data into multilines, the first containing the header, the second the data.
-- https://github.com/kerkomen/rosalind-haskell/blob/master/stronghold/GC.hs
parseFastaMultiline :: C.ByteString -> [(C.ByteString, C.ByteString)]
parseFastaMultiline f = zip (map (C.takeWhile (/='\n')) xs) (map (C.filter (/='\n') . C.dropWhile (/='\n')) xs)
    where xs = filter (\x -> C.length x > 0) . C.split '>' $ f

-- |Extract the 'fasta' data from the given 2 columns multiline array
fastaSnd :: [(C.ByteString, C.ByteString)] -> [[Char]]
fastaSnd l = [ [ x | x <- y ] | y <- r ]
    where r = [ C.unpack (snd e) | e <- l ]

-- |Extract the two columns from the given array data
fastaTuples :: [(C.ByteString, C.ByteString)] -> [([Char], [Char])]
fastaTuples l = [ toTuple e | e <- l ]
    where toTuple (a1, a2) = (C.unpack a1, C.unpack a2)

printMatrix :: (Foldable t, Show a) => t[a] -> IO()
printMatrix = mapM_ ((putStrLn . unwords) . map show)