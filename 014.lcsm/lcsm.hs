import Common
import Data.List
import Data.Maybe (catMaybes)

sharedMotif :: Int -> [[Char]] -> Either [Char] [Char]
sharedMotif minMatchLength lines = case lines of
  [] -> Left "Empty list"
  [x] -> Left "Not enough lines"
  (x : [y]) -> Right (findMaxMotif minMatchLength x y)
  (x : y : z) -> Right []

findMaxMotif :: Int -> [Char] -> [Char] -> [Char]
findMaxMotif matchLenght line1 line2 =
  head (catMaybes [match s | s <- [0 .. (length line1 - matchLenght)]])
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
  -- print (findMatch 8 1 "GACTTACA" "TAGACCA")
  let input = "/home/luca/Downloads/rosalind/014/lcsm.txt"
  let output = "/home/luca/Downloads/rosalind/014/lcsm_res.txt"
  lines <- fastaSnd <$> readFastaLines input
  let maxMotif = sharedMotif 2 lines
  print maxMotif