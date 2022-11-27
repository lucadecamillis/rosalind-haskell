import Common
import Data.List
import Data.Maybe (catMaybes)

sharedMotif :: Int -> [[Char]] -> Either [Char] [[Char]]
sharedMotif minMatchLength lines = case lines of
  [] -> Left "Empty list"
  [x] -> Left "Not enough lines"
  (x : [y]) -> Right (findMotifs minMatchLength x y)
  (x : y : z) -> Right (findMotifs minMatchLength x y)

findMotifs :: Int -> [Char] -> [Char] -> [[Char]]
findMotifs matchLenght line1 line2 =
  let matches = catMaybes [match s | s <- [0 .. (length line1 - matchLenght)]]
  in 
    if not (null matches)
    then matches ++ findMotifs (matchLenght + 1) line1 line2
    else []
  where
    match s = findMatch s matchLenght line1 line2

findMatch :: Int -> Int -> [Char] -> [Char] -> Maybe [Char]
findMatch startChar matchLenght line1 line2 =
  subStr startChar matchLenght line1 >>= infixOf line2
  where
    infixOf line candidate =
      if candidate `isInfixOf` line
        then Just candidate
        else Nothing
    subStr start count line =
      if start >= 0 && count + start <= length line
        then Just (take count (drop start line))
        else Nothing

main :: IO ()
main = do
  let input = "/home/luca/Downloads/rosalind/014/lcsm.txt"
  let output = "/home/luca/Downloads/rosalind/014/lcsm_res.txt"
  lines <- fastaSnd <$> readFastaLines input
  let maxMotif = sharedMotif 2 lines
  print maxMotif