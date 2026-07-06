import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.List             (sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map readInt)
        >>> map (\[t,d,c] -> (t+d,c))
        >>> sort
        >>> paint 0
        >>> print
    )

paint :: Int -> [(Int,Int)] -> Int
paint r [] = r
paint r ((t,c):xs) | r >= t    = paint (r+c) xs
                   | otherwise = paint (t+c) xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
