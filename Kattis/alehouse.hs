import           Control.Arrow         ((>>>))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.List             (scanl', sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    k <- fmap (C.words >>> last >>> readInt) C.getLine
    C.getContents >>= (
                C.lines
            >>> concatMap (
                        C.words
                    >>> map readInt
                    >>> (\(a:b:_) -> [(a, 1), (b + k + 1, -1)])
                )
            >>> sort
            >>> map snd
            >>> scanl' (+) 0
            >>> maximum
            >>> print
        )

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
