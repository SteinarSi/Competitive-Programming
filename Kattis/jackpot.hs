import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map readInteger)
        >>> parse
        >>> map (solve 1)
        >>> unlines
        >>> putStr
    )

solve :: Integer -> [Integer] -> String
solve m [] = show m
solve m (x:xs) | lcm m x <= 10^9 = solve (lcm m x) xs
               | otherwise   = "More than a billion."

parse :: [[Integer]] -> [[Integer]]
parse []         = []
parse (_:xs:xss) = xs : parse xss

readInteger :: C.ByteString -> Integer
readInteger = C.readInteger >>> fromJust >>> fst
