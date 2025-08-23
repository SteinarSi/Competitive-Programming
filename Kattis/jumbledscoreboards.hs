import           Control.Arrow         ((>>>))
import           Control.Monad         (ap)
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map readInt)
        >>> ap (zipWith (\[a,b] [x,y] -> a<=x && b<=y)) (drop 1)
        >>> and
        >>> bool "no" "yes"
        >>> putStrLn
    )

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
