import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map readInt >>> solve >>> show)
        >>> unlines
        >>> putStr
    )

solve :: [Int] -> Int
solve [yp,lp,ys,ls] | yp < lp   = min y l
                    | otherwise = min w (min c y)
  where
    y = yp*ys
    l = lp*ls
    w = ys * (yp-lp) + l
    c = ls * (yp-lp) + yp*ls

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
