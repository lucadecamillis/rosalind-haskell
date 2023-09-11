import Common
import Https
import Text.Regex.TDFA

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
      let content = fastaSnd $ parseFastaLines $ fst httpResult
      let result = if null content then Nothing else emptyList (head content)
      return result

findMotif :: [Char] -> [Int]
findMotif p = do
  let matches = getAllMatches (p =~ "REGEX") :: [(Int, Int)]
  fst <$> matches 

findMotifIO :: [Char] -> IO ([Char], [Int])
findMotifIO id = do
  protein <- downloadProtein id
  let occurrences = maybeGetValueOrDefault (findMotif <$> protein) []
  return (id, occurrences)

main :: IO ()
main = do
  input <- getDesktopPath "016.mprt.txt"
  lines <- readLines input
  result <- mapM findMotifIO lines
  print result