import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> tail
        >>> map readInt
        >>> helpKingWhomp 0
        >>> print
    )

helpKingWhomp :: Int -> [Int] -> Int
helpKingWhomp r []       = r
helpKingWhomp r [x]      = r + x - 1
helpKingWhomp r (x:y:xs) = helpKingWhomp (r + max 0 (x-y-1)) (y:xs)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
