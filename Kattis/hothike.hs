import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> tail
        >>> map readInt
        >>> (\xs -> zipWith max xs (drop 2 xs))
        >>> (`zip` [1..])
        >>> minimum
        >>> (\(m,i) -> unwords (map show [i,m]))
        >>> putStrLn
    )

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
