import           Control.Arrow         ((&&&), (>>>))
import           Data.Array            (Array, listArray, range, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (sortOn)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    (n,t):xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt >>> head &&& last))
    xs
        & sortOn (\(c,t) -> (t,-c))
        & listArray (1,n)
        & solve n t
        & print

solve :: Int -> Int -> Array Int (Int,Int) -> Int
solve n t queue = dp ! (1,0)
    where
        rng = ((1,0),(n+1,t))

        dp :: Array (Int,Int) Int
        dp = listArray rng (map f (range rng))

        f :: (Int,Int) -> Int
        f (i,s) | s > t = minBound
                | i > n = 0
                | s <= tt   = max pick skip
                | otherwise = skip
            where
                pick = c + dp ! (i+1,s+1)
                skip = dp ! (i+1,s)
                (c,tt) = queue ! i

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
