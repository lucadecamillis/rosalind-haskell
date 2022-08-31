transcribe2Rna :: [Char] -> [Char]
transcribe2Rna e = [ if x == 'T' then 'U' else x | x <- e ]