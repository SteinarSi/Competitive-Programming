import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    seq <- C.getContents <&> (
                C.words
            >>> tail
            >>> map readInt
            >>> gis 0
        )
    print (length seq)
    map (show >>> C.pack) seq
        & C.unwords
        & C.putStrLn

gis :: Int -> [Int] -> [Int]
gis _ [] = []
gis g (x:xs) | x > g     = x : gis x xs
             | otherwise =     gis g xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
