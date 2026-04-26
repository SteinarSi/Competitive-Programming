import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import qualified Data.IntSet           as S
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> drop 1
        >>> map readInt
        >>> bidrod S.empty
        >>> map show
        >>> unwords
        >>> putStrLn
    )

bidrod :: S.IntSet -> [Int] -> [Int]
bidrod seen [] = []
bidrod seen (x:xs)
    | S.member x seen = bidrod seen xs
    | otherwise       = x : bidrod (S.insert x seen) xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
