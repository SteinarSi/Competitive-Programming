import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.List             (foldl1')
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> map (C.words >>> map readInt >>> (\(a:b:_) -> (a,b)))
        >>> zip [1..]
        >>> foldl1' fight
        >>> fst
        >>> print
    )

fight :: (Int, (Int,Int)) -> (Int, (Int,Int)) -> (Int, (Int,Int))
fight (i, (h1,s1)) (j, (h2,s2)) | s1 >= h2 = (i, (h1,s1))
                                | otherwise = fight (j, (h2-s1,s2)) (i, (h1,s1))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
