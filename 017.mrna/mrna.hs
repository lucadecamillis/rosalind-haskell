import Rna

count :: AminoAcid -> Int
count = length . f
    where
        f x = filter (==x) (map snd Rna.codonTable)

compute :: [Char] -> Int
compute e = 3 * foldl accumulate 1 e `mod` mod_p
    where
        accumulate b a = occurrence a * b `mod` mod_p
        occurrence x = count (read [x]::AminoAcid)
        mod_p = 1000000

main :: IO ()
main = do
    let input = "MA"
    let count = compute input
    print ("result " ++ show count)