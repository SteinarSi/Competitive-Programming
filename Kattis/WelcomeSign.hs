import qualified Data.ByteString.Char8 as C
import Control.Arrow ((>>>), (&&&))
import Data.Maybe (fromJust)
import Data.Functor ((<&>))
import Data.Tuple (swap)
import Data.Function ((&))

main :: IO ()
main = do
    (w,xs) <- C.getContents <&> (
                C.lines
            >>> (head >>> C.words >>> last >>> readInt)
                    &&&
                drop 1
        )

    format w 1 xs
        & C.unlines
        & C.putStr

format :: Int -> Int -> [C.ByteString] -> [C.ByteString]
format w i [] = []
format w i (x:xs) = C.replicate l '.' <> x <> C.replicate r '.' : format w i' xs
    where
        d = w - C.length x
        ab = (d `div` 2, (d+1) `div` 2)
        (l,r) | odd i     = ab
              | otherwise = swap ab
        i' | odd d     = succ i
           | otherwise = i

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
