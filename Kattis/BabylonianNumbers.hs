import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.List             (span)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> mapM_ (C.unpack
            >>> (++",")
            >>> fill
            >>> splitOn ','
            >>> map read
            >>> reverse
            >>> zipWith (*) (map (60^) [0..])
            >>> sum
            >>> print
            )
    )

fill :: String -> String
fill ""           = ""
fill (',':',':xs) = ',' : '0' : fill (',':xs)
fill (x:xs)       = x : fill xs

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn p (x:xs) | p == x    = splitOn p xs
                 | otherwise = let (a, b) = span (p/=) xs
                               in  (x:a) : splitOn p b
