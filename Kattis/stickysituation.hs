import           Control.Arrow         ((>>>))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.List             (sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> tail
        >>> map readInt
        >>> sort
        >>> stickySituation
        >>> bool "impossible" "possible"
        >>> putStrLn
    )

stickySituation :: [Int] -> Bool
stickySituation (x:y:z:xs) = x + y > z || stickySituation (y:z:xs)
stickySituation _          = False

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
