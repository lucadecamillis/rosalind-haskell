import Common

translate :: [Char] -> Either [Char] [AminoAcid]
translate e = sequence [rnaCodonTable a | a <- rna]
  where
    rna = splitEvery 3 e

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

data AminoAcid = A | R | N | D | C | Q | E | G | H | I | L | K | M | F | P | S | T | W | Y | V | Stop deriving (Show)

rnaCodonTable e = case e of
  "UUU" -> Right F
  "CUU" -> Right L
  "AUU" -> Right I
  "GUU" -> Right V
  "UUC" -> Right F
  "CUC" -> Right L
  "AUC" -> Right I
  "GUC" -> Right V
  "UUA" -> Right L
  "CUA" -> Right L
  "AUA" -> Right I
  "GUA" -> Right V
  "UUG" -> Right L
  "CUG" -> Right L
  "AUG" -> Right M
  "GUG" -> Right V
  "UCU" -> Right S
  "CCU" -> Right P
  "ACU" -> Right T
  "GCU" -> Right A
  "UCC" -> Right S
  "CCC" -> Right P
  "ACC" -> Right T
  "GCC" -> Right A
  "UCA" -> Right S
  "CCA" -> Right P
  "ACA" -> Right T
  "GCA" -> Right A
  "UCG" -> Right S
  "CCG" -> Right P
  "ACG" -> Right T
  "GCG" -> Right A
  "UAU" -> Right Y
  "CAU" -> Right H
  "AAU" -> Right N
  "GAU" -> Right D
  "UAC" -> Right Y
  "CAC" -> Right H
  "AAC" -> Right N
  "GAC" -> Right D
  "UAA" -> Right Stop
  "CAA" -> Right Q
  "AAA" -> Right K
  "GAA" -> Right E
  "UAG" -> Right Stop
  "CAG" -> Right Q
  "AAG" -> Right K
  "GAG" -> Right E
  "UGU" -> Right C
  "CGU" -> Right R
  "AGU" -> Right S
  "GGU" -> Right G
  "UGC" -> Right C
  "CGC" -> Right R
  "AGC" -> Right S
  "GGC" -> Right G
  "UGA" -> Right Stop
  "CGA" -> Right R
  "AGA" -> Right R
  "GGA" -> Right G
  "UGG" -> Right W
  "CGG" -> Right R
  "AGG" -> Right R
  "GGG" -> Right G
  _ -> Left ("Cannot match " ++ e)