import Data.Char (ord, chr)
import Numeric (readInt, showIntAtBase)

main :: IO ()
main = getLine >>= (\[x, d] -> putStrLn (reverse (replicate (length x - length (tot x d)) 'A' ++ tot x d))) . words
    where tot x d = showIntAtBase 2 (chr . (+ 65)) ((read d +) . fst . head . readInt 2 (`elem` "AB") (subtract 65 . ord) $ reverse x) ""
