import           Control.Arrow         ((>>>))
import           Control.Monad         (join)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.IntSet           (IntSet, delete, empty, findMax, insert,
                                        member)
import qualified Data.IntSet           as S
import           Data.List             (elemIndex)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> tail
        >>> map (C.readInt >>> fromJust >>> fst)
        >>> (\xs -> solve empty empty xs >>= (`elemIndex` xs))
        >>> maybe "none" (succ >>> show)
        >>> putStrLn
    )

solve :: IntSet -> IntSet -> [Int] -> Maybe Int
solve seen banned [] | S.null seen = Nothing
                     | otherwise = Just (findMax seen)
solve seen banned (x:xs) | member x banned = solve seen banned xs
                         | member x seen = solve (delete x seen) (insert x banned) xs
                         | otherwise = solve (insert x seen) banned xs
