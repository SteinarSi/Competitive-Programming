import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> init
        >>> map (C.unpack >>> reverse >>> solve 'A' 'B' 'C' >>> show)
        >>> unlines
        >>> putStr
    )

solve :: Char -> Char -> Char -> String -> Int
solve _ _ _ ""                   = 0
solve start target temp (x:xs)
    | x == target = solve temp target start xs
    | otherwise = 2^length xs + solve start temp target xs
