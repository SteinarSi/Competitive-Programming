import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.List             (sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map readInt)
        >>> parse
        >>> map (uncurry (redistribute 1) >>> show)
        >>> unlines
        >>> putStr
    )

redistribute :: Int -> Int -> [Int] -> Int
redistribute m r []  = error "bruh"
redistribute m r [x] = x + r `div` m
redistribute m r (x:y:xs) | (y-x)*m <= r = redistribute (m+1) (r - (y-x)*m) (y:xs)
                          | otherwise    = x + r `div` m

parse :: [[Int]] -> [(Int, [Int])]
parse []           = []
parse (_:ws:ls:xs) = (sum (zipWith (-) ws ls), sort ls) : parse xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
