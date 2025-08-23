import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         (on)
import           Data.List             (maximumBy)
import           Data.Map.Strict       (assocs, empty, insertWith)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> map (C.words >>> (\(a:b:_) -> (a,readInt b)))
        >>> foldr (uncurry (insertWith (+))) empty
        >>> assocs
        >>> maximumBy (compare `on` snd)
        >>> fst
        >>> C.putStrLn
    )

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
