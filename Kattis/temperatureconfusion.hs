import qualified Data.ByteString.Char8 as C
import           Data.Ratio               (denominator, numerator, (%))
import           Data.Functor             ((<&>))
import           Control.Arrow            ((>>>), (***), (&&&))
import           Data.Maybe               (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.span (/='/') 
        >>> readInt *** (C.tail >>> readInt) 
        >>> uncurry (%)
        >>> subtract 32
        >>> (*(5%9))
        >>> numerator &&& denominator
        >>> (show >>> (++"/") >>> (++)) *** show
        >>> uncurry ($)
        >>> C.pack
        >>> C.putStrLn
    )

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
