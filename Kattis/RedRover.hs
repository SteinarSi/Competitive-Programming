import           Control.Category      ((>>>))
import           Control.Monad         (guard)
import qualified Data.ByteString.Char8 as C
import           Data.Function         (on, (&))
import           Data.List             (groupBy, sort)

main :: IO ()
main = do
    text <- C.getLine
    substrings text
        & sort
        & groupBy (on (==) fst)
        & map (
                removeOverlaps
            >>> map fst
            >>> (\xs@(x:_) -> C.length text + C.length x - (length xs * (C.length x-1)))
            )
        & (C.length text:)
        & minimum
        & print

removeOverlaps :: [(a,(Int,Int))] -> [(a,(Int,Int))]
removeOverlaps [] = []
removeOverlaps [x] = [x]
removeOverlaps ((a,(s,t)):(b,(x,y)):xs) | x < t = removeOverlaps ((a,(s,t)):xs)
                                        | otherwise = (a,(s,t)) : removeOverlaps ((b,(x,y)):xs)

substrings :: C.ByteString -> [(C.ByteString, (Int,Int))]
substrings xss = do
    (s,xs) <- zip [0..] (C.tails xss)
    (t,x) <- zip [s..] (C.inits xs)
    guard (C.length x >= 2 && C.length x <= (C.length xss `div` 2 + 1))
    pure (x,(s,t))
