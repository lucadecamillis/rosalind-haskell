import Common
import Data.List
import Data.Ord

computeGC :: [Char] -> Double
computeGC e = 100.0 * fromIntegral (content e) / fromIntegral (length e)
    where
        content = length . filter cORg
        cORg x = x == 'C' || x == 'G'

computeMaxGC :: [([Char], [Char])] -> ([Char], Double)
computeMaxGC l = maximumBy (comparing snd) [(t, computeGC e) | (t, e) <- l]

main :: IO ()
main = do
    let input = "/home/luca/Desktop/rosalind_gc.txt"
    lines <- fastaTuples <$> readFastaLines input
    let maxGC = computeMaxGC lines
    print maxGC