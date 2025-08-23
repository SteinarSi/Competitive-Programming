import           Data.Char (chr, ord)

main :: IO ()
main = do
    n:_ <- fmap (map read . words) getLine
    [end, cipher] <- fmap (map (reverse . map toIndex) . lines) getContents
    putStrLn . map toChar . drop n . reverse $ decrypt end cipher

decrypt :: [Int] -> [Int] -> [Int]
decrypt first message = first ++ zipWith (((`mod` 26) .) . (-)) message (decrypt first message)

toIndex :: Char -> Int
toIndex c = ord c - ord 'a'

toChar :: Int -> Char
toChar = chr . (ord 'a' +)
