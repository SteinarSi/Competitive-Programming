import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map (readInt >>> fromIntegral))
        >>> parse
        >>> map (solve >>> show)
        >>> unlines
        >>> putStr
    )

solve :: (Double,[Double]) -> Double
solve (p,xs) = sum (map (\s -> (p * (s/total))^2 / s) xs)
  where
    total = sum xs

parse :: [[Double]] -> [(Double,[Double])]
parse []             = []
parse ([_,p]:xs:xss) = (p,xs) : parse xss

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
