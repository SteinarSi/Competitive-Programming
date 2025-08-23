import           Data.Char (chr, ord)

main :: IO ()
main = do
    [message, key] <- fmap (map (map (subtract (ord 'A') . ord)) . lines) getContents

    let decrypted = zipWith (\x y -> (x - y) `mod` 26) message (key ++ decrypted)

    putStrLn $ map (chr . (ord 'A'+)) decrypted
