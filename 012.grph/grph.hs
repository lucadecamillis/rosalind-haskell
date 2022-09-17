import Common


main :: IO ()
main = do
    let input = "/home/luca/Desktop/grph.txt"
    let output = "/home/luca/Desktop/grph_res.txt"
    lines <- fastaTuples <$> readFastaMultiline input
    print lines