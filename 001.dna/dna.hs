import System.Environment
import Data.Map

-- Counting DNA Nucleotides
countNucleotides :: String -> [(Char, Integer)]
countNucleotides e = toList $ fromListWith (+) [(c, 1) | c <- e]

main :: IO ()
main = do
    (input:_) <- getArgs
    print $ countNucleotides input