import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.List             (find, partition)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> map (C.unpack >>> solve >>> show)
        >>> unlines
        >>> putStr
    )

solve :: String -> Int
solve xs = maybe (length xs - 1) pred (find (selfsimilar xs >>> not) [1 .. length xs - 1])

selfsimilar :: String -> Int -> Bool
selfsimilar = substrings >>> (>>> similar)
  where
    similar :: [String] -> Bool
    similar [] = True
    similar (s:ss) = let (a,b) = partition (s==) ss
                     in  not (null a) && similar b

substrings :: String -> Int -> [String]
substrings xs d = split (length xs) xs
  where
    split ::  Int -> String -> [String]
    split l ys | l > d     = take d ys : split (l-1) (tail ys)
               | otherwise = [ys]
