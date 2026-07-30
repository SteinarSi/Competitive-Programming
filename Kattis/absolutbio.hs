import           Control.Arrow         ((>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (STUArray, newArray, readArray,
                                        writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n,_]:xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))
    runST (newArray (1,n) False >>= \kino -> solve kino [] xs)
        & unlines
        & putStr

solve :: STUArray s Int Bool -> [String] -> [[Int]] -> ST s [String]
solve kino ret [] = pure (reverse ret)
solve kino ret ([1, i]:xs) = do
    u <- readArray kino i
    if u
        then solve kino ("Upptagen":ret) xs
        else writeArray kino i True >> solve kino ("Ledig":ret) xs
solve kino ret ([2, i, j]:xs) = do
    u <- readArray kino j
    if u
        then solve kino ("Upptagen":ret) xs
        else writeArray kino i False >> writeArray kino j True >> solve kino ("Ledig":ret) xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
