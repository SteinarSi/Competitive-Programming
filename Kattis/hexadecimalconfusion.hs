import           Control.Arrow ((>>>))
import           Data.Bool     (bool)
import           Data.List     (transpose)
import           Numeric       (readHex)
import           Text.Printf   (printf)

main :: IO ()
main = getContents >>= (
            lines
        >>> drop 1
        >>> map parse
        >>> transpose
        >>> map (sum >>> printf "%X")
        >>> unlines
        >>> putStr
    )

parse :: String -> [Int]
parse xs = [hi,lo]
  where
    [(hi,_)] = readHex (replace '8' 'B' (replace '0' 'D' xs))
    [(lo,_)] = readHex $ case xs of
        "0"    -> "0"
        "D"    -> "0"
        '0':ys -> 'D' : replace 'B' '8' (replace 'D' '0' ys)
        'D':ys -> 'D' : replace 'B' '8' (replace 'D' '0' ys)
        _      -> replace 'B' '8' (replace 'D' '0' xs)

replace :: Char -> Char -> String -> String
replace a b = map (\x -> bool x b (x==a))
