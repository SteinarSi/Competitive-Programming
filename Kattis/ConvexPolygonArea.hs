import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> mapM_ (C.words
            >>> tail
            >>> map readNum
            >>> parse
            >>> solve
            >>> print
        )
    )

solve :: [(Double,Double)] -> Double
solve xs@(x:_) = shoelace (xs ++ [x])

shoelace :: [(Double, Double)] -> Double
shoelace xs = abs (sum (zipWith (\(x1,y1) (x2,y2) -> (y1+y2) * (x1-x2)) xs (tail xs))) / 2

parse :: [Double] -> [(Double,Double)]
parse []       = []
parse (x:y:xs) = (x,y) : parse xs

readNum :: Num a => C.ByteString -> a
readNum = C.readInt >>> fromJust >>> fst >>> fromIntegral
