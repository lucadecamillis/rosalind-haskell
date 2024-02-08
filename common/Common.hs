module Common where

import qualified Data.Bifunctor
import qualified Data.ByteString.Char8 as C
import System.Directory as D
import System.FilePath
import Text.Read (Lexeme (Char))

-- | Read lines in the given file
readLines :: FilePath -> IO [[Char]]
readLines path = lines . C.unpack <$> C.readFile path

-- | Read 'fasta' file data into a 2 column data structure.
--  The second column contains the actual data
readFastaLines :: FilePath -> IO [(C.ByteString, C.ByteString)]
readFastaLines path = parseFastaLines <$> C.readFile path

-- | Write the given array of string into the given output file
writeLines :: FilePath -> [[Char]] -> IO ()
writeLines path lines = C.writeFile path (C.pack (unlines lines))

-- | Parse fasta data into multilines, the first containing the header, the second the data.
--  https://github.com/kerkomen/rosalind-haskell/blob/master/stronghold/GC.hs
parseFastaLines :: C.ByteString -> [(C.ByteString, C.ByteString)]
parseFastaLines f = zip (map (C.takeWhile (/= '\n')) xs) (map (C.filter (/= '\n') . C.dropWhile (/= '\n')) xs)
  where
    xs = filter (\x -> C.length x > 0) . C.split '>' $ f

-- | Extract the 'fasta' data from the given 2 columns multiline array
fastaSnd :: [(C.ByteString, C.ByteString)] -> [[Char]]
fastaSnd l = [[x | x <- y] | y <- r]
  where
    r = [C.unpack v | (r, v) <- l]

-- | Extract the two columns from the given array data
fastaTuples :: [(C.ByteString, C.ByteString)] -> [([Char], [Char])]
fastaTuples l = [toTuple e | e <- l]
  where
    toTuple (a1, a2) = (C.unpack a1, C.unpack a2)

printMatrix :: (Foldable t, Show a) => t [a] -> IO ()
printMatrix = mapM_ ((putStrLn . unwords) . map show)

-- | Split the list every n elements
splitEvery _ [] = []
splitEvery n list = first : splitEvery n rest
  where
    (first, rest) = splitAt n list

-- | Get uniprot fasta URL given the protein ID
getFastaUrl :: [Char] -> [Char]
--getFastaUrl id = "https://www.uniprot.org/uniprot/" ++ id ++ ".fasta"
getFastaUrl id = "https://www.ebi.ac.uk/proteins/api/proteins/" ++ id ++ ".fasta"

-- | Return the path of a file within the user's desktop
getDesktopPath :: [Char] -> IO FilePath
getDesktopPath subPath = do
  homeDir <- D.getHomeDirectory
  return (homeDir </> "Desktop" </> subPath)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

emptyList :: [a] -> Maybe [a]
emptyList l = if null l then Nothing else Just l

maybeGetValueOrDefault :: Maybe a -> a -> a
maybeGetValueOrDefault (Just x) _ = x
maybeGetValueOrDefault Nothing defaultValue = defaultValue

note :: Maybe a -> e -> Either e a
note Nothing e = Left e
note (Just a) _ = Right a