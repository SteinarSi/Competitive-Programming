import           Control.Arrow         ((>>>))
import           Data.Bits             ((.|.))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> map C.words
        >>> zipWith skipAt [0..]
        >>> mapM_ (
                map (
                        C.readInt
                    >>> fromJust
                    >>> fst
                )
            >>> foldl (.|.) 0
            >>> print
        )
    )

skipAt :: Int -> [a] -> [a]
skipAt _ []     = error "bruh"
skipAt 0 (_:xs) = xs
skipAt i (x:xs) = x : skipAt (i-1) xs
