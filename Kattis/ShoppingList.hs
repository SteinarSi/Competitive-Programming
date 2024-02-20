import           Control.Arrow         ((>>>))
import           Control.Monad         (ap)
import qualified Data.ByteString.Char8 as C
import           Data.List             (foldl1')
import           Data.Set              (fromList, intersection, toAscList)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> map (C.words >>> fromList)
        >>> foldl1' intersection
        >>> toAscList
        >>> ap (length >>> print >>> (>>)) (mapM_ C.putStrLn)
    )
