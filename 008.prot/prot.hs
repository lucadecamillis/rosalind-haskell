import Common
import Rna

translate :: [Char] -> Either [Char] [AminoAcid]
translate e = sequence [l a | a <- rna]
  where
    rna = splitEvery 3 e
    l a = lookup a Rna.codonTable `note` ("Cannot match " ++ e)

main :: IO ()
main = do
  let input = "/home/luca/Desktop/prot.txt"
  let output = "/home/luca/Desktop/prot_res.txt"
  lines <- readLines input
  let rna = case length lines of
        1 -> translate (head lines)
        _ -> Left "Wrong number of lines"
  case rna of
    Left err -> print err
    Right r -> writeLines output [concatMap show r]