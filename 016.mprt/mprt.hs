import Common
import Https
import Text.Regex.PCRE
import Data.List

purifyId :: [Char] -> Maybe [Char]
purifyId id = if null parts then Nothing else Just (head parts)
  where
    parts = wordsWhen (== '_') id

downloadProtein :: [Char] -> IO (Maybe [Char])
downloadProtein id = do
  case purifyId id of
    Nothing -> return Nothing
    Just innerId -> do
      httpResult <- getViaRequest $ getFastaUrl innerId
      print ("Downloaded protein " ++ id ++ " size: " ++ show (snd httpResult))
      let content = fastaSnd $ parseFastaLines $ fst httpResult
      let result = if null content then Nothing else emptyList (head content)
      return result

findMotif :: [Char] -> [Int]
findMotif p = do
  let matches = getAllMatches (p =~ "(?=(N[^P]{1}[S|T]{1}[^P]{1}))([A-Z]{1})") :: [(Int, Int)]
  (+1) . fst <$> matches -- +1 here because regex find indices while we want char count

findMotifIO :: [Char] -> IO ([Char], [Int])
findMotifIO id = do
  protein <- downloadProtein id
  let occurrences = maybeGetValueOrDefault (findMotif <$> protein) []
  return (id, occurrences)

filterAndFormat :: [([Char], [Int])] -> [String]
filterAndFormat = concatMap formatPair . filterNotNull
  where
    filterNotNull r = filter (not . null . snd) r
    formatPair (e1, e2) = [e1, formatIndices e2]
    formatIndices = unwords . map show

main :: IO ()
main = do
  input <- getDesktopPath "rosalind_mprt.txt"
  lines <- readLines input
  result <- mapM findMotifIO lines
  let rows = filterAndFormat result
  let textResult = unlines rows
  putStrLn textResult