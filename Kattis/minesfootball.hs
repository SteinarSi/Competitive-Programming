import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.List             (foldl')
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words
            >>> drop 1
            >>> map readInt)
        >>> analyze minBound maxBound minBound maxBound
        >>> putStr
    )

analyze :: Int -> Int -> Int -> Int -> [[Int]] -> String
analyze ma mi sma smi [] = unlines (map show [ma,mi,sma,smi])
analyze ma mi sma smi (xs:xss) = analyze (max ma h) (min mi l) (max sma s) (min smi s) xss
  where
    (s,l,h) = foldl' (\(s,l,h) x -> (s+x, min l x, max h x)) (0,maxBound,minBound) xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
