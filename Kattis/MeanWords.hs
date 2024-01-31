import           Data.Char (chr, ord)
import           Data.List (transpose)

main :: IO ()
main = do
    meanWords <- fmap (map (map ord) .tail . words) getContents
    let longest = maximum (map length meanWords)
        encoded = map (\xs -> chr (sum xs `div` length xs)) (transpose meanWords)
    putStrLn encoded
