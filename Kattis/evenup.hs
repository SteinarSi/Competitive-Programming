import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> tail
        >>> map (readInt >>> odd)
        >>> play [] False
        >>> print
    )

play :: [Bool] -> Bool -> [Bool] -> Int
play rs False []                   = length rs
play rs True  []                   = play [] False rs
play rs b     [x]                  = play (x:rs) b []
play rs b     (x:y:xs) | x == y    = play rs True xs
                       | otherwise = play (x:rs) b (y:xs)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
