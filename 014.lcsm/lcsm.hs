import Common

sharedMotif :: Int -> [[Char]] -> Either [Char] [Char]
sharedMotif minMatchLength lines = case lines of
    [] -> Left "Empty list"
    [x] -> Left "Not enough lines"
    (x : [y]) -> Right (findMaxMotif minMatchLength x y)
    (x : y : z) -> Right []

findMaxMotif :: Int -> [Char] -> [Char] -> [Char]
findMaxMotif matchLenght line1 line2 = []

main :: IO ()
main = do
    let input = "/home/luca/Downloads/rosalind/014/rosalind_lcsm.txt"
    let output = "/home/luca/Downloads/rosalind/014/lcsm_res.txt"
    lines <- fastaSnd <$> readFastaLines input
    let maxMotif = sharedMotif 2 lines
    print maxMotif