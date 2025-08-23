import           Control.Arrow         ((>>>))
import           Data.ByteString       (sort)
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> map (C.words >>> (\(a:b:c:_) -> (- (readInt b * readInt c), a)))
        >>> minimum
        >>> snd
        >>> C.putStrLn
    )

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
