import Common
import Data.List
import Data.Ord

computeGC :: [Char] -> Double
computeGC e = fromIntegral (length $ filter (\x -> x == 'C' || x == 'G') e) * 100 / fromIntegral (length e)

computeMaxGC :: [([Char], [Char])] -> ([Char], Double)
computeMaxGC l = maximumBy (comparing snd) [(t, computeGC e) | (t, e) <- l]

main :: IO ()
main = do
    let input = "/home/luca/Desktop/rosalind_gc.txt"
    lines <- fastaTuples <$> readFastaMultiline input
    let maxGC = computeMaxGC lines
    print maxGC