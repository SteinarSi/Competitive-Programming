import           Control.Arrow         ((>>>))
import           Data.Array.Unboxed    (UArray, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.List             (sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n, q] <- C.getLine <&> (C.words >>> map readInt)
    psum <- C.getLine <&> (C.words
            >>> map readInt
            >>> sort
            >>> scanl (+) 0
            >>> listArray (0, n)
        ) :: IO (UArray Int Int)
    C.getContents >>= (C.words
            >>> mapM_ (readInt >>> (psum!) >>> print)
        )

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
