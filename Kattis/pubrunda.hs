import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         (on)
import           Data.List             (maximumBy)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> map (C.words >>> (\(a:b:c:_) -> ((1+readInt b) * readInt c, a)))
        >>> reverse
        >>> maximumBy (compare `on` fst)
        >>> (\(w,n) -> C.unwords [n,C.pack (show w)])
        >>> C.putStrLn
    )

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
