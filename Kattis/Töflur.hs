import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.IntSet           (empty, insert, toAscList)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (C.words
        >>> tail
        >>> map readInt
        >>> foldr insert empty
        >>> toAscList
        >>> (\xs -> zipWith (-) xs (tail xs))
        >>> map (^2)
        >>> sum
        >>> print
    )

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
