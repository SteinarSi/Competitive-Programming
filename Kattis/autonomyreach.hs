import           Control.Arrow         ((>>>))
import           Control.Monad         (filterM)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (STUArray, inRange, newArray, readArray,
                                        writeArray)
import           Data.Array.Unboxed    (UArray, assocs, bounds, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    r:c:xs <- C.getContents <&> C.words
    let rng = ((1,1),(readInt r, readInt c))
        grid = listArray rng (concatMap C.unpack xs)
    print $ runST $ do
        seen <- newArray rng False
        assocs grid
            & filter (snd >>> (=='S'))
            & mapM_ (fst >>> search grid seen)
        assocs grid
            & filter (snd >>> (=='P'))
            & filterM (fst >>> readArray seen)
            <&> length

search :: UArray (Int,Int) Char -> STUArray s (Int,Int) Bool -> (Int,Int) -> ST s ()
search grid seen (y,x) = [(y-1,x),(y+1,x),(y,x-1),(y,x+1)]
    & filter (inRange (bounds grid))
    & filter ((grid!) >>> (`elem` ".CP"))
    & filterM (\p -> readArray seen p >>= \s -> if s then pure False else writeArray seen p True >> pure True)
    >>= mapM_ (search grid seen)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
