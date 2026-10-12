import           Control.Arrow         ((***), (>>>))
import           Control.Monad         (when)
import           Data.Array.Base       (elems)
import           Data.Array.ST         (newArray, runSTUArray, writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map readInt)
        >>> parse
        >>> map (solve >>> show)
        >>> unlines
        >>> putStr
    )

solve :: (Int,[[Int]]) -> Int
solve (n,xs)
    | any (\[x,y] -> y < x) xs = -1
    | otherwise                = (runSTUArray $ do
            ret <- newArray (1,n) True
            writeArray ret 1 False
            mapM_ (\[x,y] -> when (x+1 == y) (writeArray ret y False)) xs
            pure ret)
        & elems
        & filter id
        & length

parse :: [[Int]] -> [(Int,[[Int]])]
parse [] = []
parse ([n,m]:xs) = splitAt m xs
    & (n,) *** parse
    & uncurry (:)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
