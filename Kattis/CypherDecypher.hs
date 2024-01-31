import           Data.Char (chr, ord)

main :: IO ()
main = do
    secret <- fmap (map (read . pure)) getLine
    messages <- fmap (tail . lines) getContents
    mapM_ (putStrLn . encode secret) messages

encode :: [Int] -> String -> String
encode secret message = map toChar $ zipWith (*) secret (map toInt message)
    where
        toInt :: Char -> Int
        toInt a = (ord a - ord 'A') `mod` 26

        toChar :: Int -> Char
        toChar = chr . (ord 'A' +) . (`mod` 26)
