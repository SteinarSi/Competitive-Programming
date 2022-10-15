main :: IO ()
main = do
    input <- getLine
    let wordss = getWordList ((length input)`div`3) input
    if wordss!!0 == wordss!!1 then putStrLn (wordss!!0) 
    else if wordss!!0 == wordss!!2 then putStrLn (wordss!!0)
    else putStrLn (wordss!!1)


getWordList :: Int -> String -> [String]
getWordList _ "" = []
getWordList n s = take n s : getWordList n (drop n s)