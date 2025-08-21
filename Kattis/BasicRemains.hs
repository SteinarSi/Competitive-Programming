import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (digitToInt, intToDigit)
import           Numeric               (readInt, showIntAtBase)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> init
        >>> map (C.unpack
            >>> words
            >>> solve)
        >>> unlines
        >>> putStr
    )

solve :: [String] -> String
solve [b,p,m] = format (parse p `mod` parse m)
  where
    format :: Integer -> String
    format x = showIntAtBase (read b) intToDigit x ""

    parse :: String -> Integer
    parse = readInt (read b) (const True) digitToInt
        >>> head
        >>> fst
        >>> fromIntegral
