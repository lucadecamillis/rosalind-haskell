import Rna

count :: AminoAcid -> Int
count = length . f
    where
        f x = filter (==x) (map snd Rna.codonTable)

main :: IO ()
main = do
    let input = "MA"
    let d = count Rna.A
    print ("result " ++ show d)