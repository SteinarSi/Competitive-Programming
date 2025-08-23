import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> drop 1
        >>> map readInt
        >>> reverse
        >>> genocide 0
        >>> print
    )

genocide :: Int -> [Int] -> Int
genocide r []  = r
genocide r [_] = r
genocide r (0:_:_) = 1 -- (Thanos unalives himself)
genocide r (x:y:xs) | y >= x    = genocide (r + y - (x-1)) (x-1:xs)
                    | otherwise = genocide r (y:xs)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
