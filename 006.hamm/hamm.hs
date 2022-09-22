import Common

hammingDistance e1 e2 = length [x | (x, y) <- zip e1 e2, x /= y]

main :: IO ()
main = do
  let input = "/home/luca/Desktop/hamm.txt"
  let output = "/home/luca/Desktop/hamm_res.txt"
  lines <- readLines input
  let distance = case lines of
        [e1, e2] -> Right (hammingDistance e1 e2)
        _ -> Left "Wrong number of lines"
  print distance