import System.IO

handleInputs :: [String] -> IO ()
handleInputs [] = return ()
handleInputs (x:xs) = do
    putStrLn (getCode x)
    handleInputs xs

getInputs :: Int -> IO [String]
getInputs 0 = return []
getInputs n = do
    x <- getLine
    xs <- getInputs (n-1)
    return (x:xs)

getCode :: String -> String
getCode [] = ""
getCode (x:xs) = case soundcode x 6 of
        Nothing -> getCode xs
        Just i -> (show i) ++ (getCode (dropWhile (\c-> soundcode c 6 == Just i) xs))

soundcode :: Char -> Int -> Maybe Int
soundcode c 0 = Nothing
soundcode c x = if c `elem` (soundex!!x) then Just x else soundcode c (x-1)

soundex = ["___", "BFPV", "CGJKQSXZ", "DT", "L", "MN", "R"]

inter :: Int -> IO ()
inter 0 = return ()
inter n = do
    input <- getLine
    hFlush stdout
    putStrLn (getCode input)
    hFlush stdout
    inter (n-1)

main = do
    content <- getContents
    let lin = lines content
    handleInputs lin