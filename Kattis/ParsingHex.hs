import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (digitToInt, isDigit, ord, toUpper)
import           Data.Function         ((&))
import           Data.Functor          ((<&>))

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> concatMap (C.unpack >>> search)
        >>> mapM_ (format >>> putStrLn)
    )

format :: String -> String
format x = drop 2 x
    & map r
    & reverse
    & zipWith (*) (map (16^) [0..])
    & sum
    & show
    & ((x ++ " ") ++)
    where
        r :: Char -> Int
        r c | isDigit c = digitToInt c
            | otherwise = ord (toUpper c) - ord 'A' + 10

search :: String -> [String]
search [] = []
search ('0':x:y:xs) | elem x "xX" && isHex y = let h = y : take 7 (takeWhile isHex xs)
                                               in  ('0' : x : h) : search (drop (length h) xs)
                    | otherwise = search xs
search (_:xs) = search xs

isHex :: Char -> Bool
isHex c = isDigit c || 'A' <= c && c <= 'F' || 'a' <= c && c <= 'f'
