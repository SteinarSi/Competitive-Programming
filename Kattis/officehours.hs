import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         (on)
import           Data.List             (maximumBy)
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromJust)
import           Text.Printf           (printf)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> concatMap (C.words >>> \(_:d:_:xs) -> map (readInt >>> (d,) >>> (,1)) xs)
        >>> M.fromListWith (+)
        >>> M.assocs
        >>> maximumBy (compare `on` snd)
        >>> (\((d,t),_) -> printf "Your professor should host office hours %s @ %.2d:00\n" (C.unpack d) t)
    )

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
