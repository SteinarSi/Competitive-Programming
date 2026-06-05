import           Control.Arrow         ((>>>))
import           Data.Array.Unboxed    (Array, UArray, assocs, elems, inRange,
                                        listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    r:c:xs <- C.getContents <&> (C.words >>> map readInt)

    let rng = ((1,1),(r,c))

        grid :: UArray (Int,Int) Int
        grid = listArray rng xs

        dp :: Array (Int,Int) Int
        dp = listArray rng (map f (assocs grid))
          where
            f ((y,x),v) = [(y-1,x-1),(y-1,x),(y-1,x+1),(y,x+1),(y+1,x+1),(y+1,x),(y+1,x-1),(y,x-1)]
                & filter (inRange rng)
                & filter ((grid!) >>> (<v))
                & map (dp!)
                & (0:)
                & maximum
                & succ

    print (maximum (elems dp))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
