import Common

hammingDistanceFromLines :: [[Char]] -> Either String Int
hammingDistanceFromLines [] = Left "No lines"
hammingDistanceFromLines [e1, e2] = Right (hammingDistance e1 e2)
hammingDistanceFromLines (_:_) = Left "Too many lines"

hammingDistance :: [Char] -> [Char] -> Int
hammingDistance e1 e2 = length [ x | (x, y) <- zip e1 e2, x /= y]

main :: IO ()
main = do
    let input = "/home/luca/Desktop/hamm.txt"
    let output = "/home/luca/Desktop/hamm_res.txt"
    lines <- readLines input
    let distance = hammingDistanceFromLines lines
    print distance