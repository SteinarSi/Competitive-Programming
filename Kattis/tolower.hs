import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (isLower)
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [p, t] <- C.getLine <&> (C.words >>> map (C.readInt >>> fromJust >>> fst))
    C.getContents >>= (
                C.lines
            >>> chunksOf t
            >>> filter (all (C.tail >>> C.all isLower))
            >>> length
            >>> print
        )

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = a : chunksOf k b
    where (a,b) = splitAt k xs
