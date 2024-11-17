import           Control.Arrow         (first, (>>>))
import           Control.Monad         (guard)
import           Data.Array.Unboxed    (UArray, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Foldable         (minimumBy)
import           Data.Function         (on, (&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:xs <- C.getContents <&> (C.words >>> map readInt)

    let grid = listArray ((1,1),(n,n)) xs :: UArray (Int,Int) Int

        triangles = (0, (1,1,1)) : do
            i <- [1..n-2]
            j <- [i+1..n-1]
            guard (grid ! (i,j) > 0)
            k <- [j+1..n]
            pure (grid ! (i,j) * grid ! (i,k) * grid ! (j,k), (i,j,k))

        (a,b,c) = triangles
            & minimumBy (compare `on` first negate)
            & snd

    [a,b,c]
        & map (show >>> C.pack)
        & C.unwords
        & C.putStrLn

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
