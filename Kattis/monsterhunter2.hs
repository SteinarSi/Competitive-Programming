import           Control.Arrow         ((&&&), (***), (>>>))
import           Control.Monad         (forM_)
import           Data.Array.ST         (MArray, newArray, readArray,
                                        runSTUArray, writeArray)
import           Data.Array.Unboxed    (Ix, UArray, assocs, bounds, elems,
                                        listArray, (!))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.IntSet           as S
import           Data.List             (partition)
import           Data.Maybe            (fromJust)
import           Text.Printf           (printf)

main :: IO ()
main = do
    r:c:xs <- C.getContents <&> C.words
    concatMap C.unpack xs
        & listArray ((1,1),(readInt r,readInt c))
        & solve
        & printf "%.5f\n"

solve :: UArray (Int,Int) Char -> Double
solve grid = simulate initial 1
  where
    rng = bounds grid
    (trap,open) = assocs grid
        & filter (snd >>> (/='#'))
        & partition (snd >>> (=='O'))
        & map fst *** map (\((y,x),_) ->
            [(y-1,x),(y+1,x),(y,x-1),(y,x+1),(y,x)]
                & filter ((grid!)>>>(/='#'))
                & (((y,x),) &&& (length >>> fromIntegral >>> (1/))))
    initial = elems grid
        & map ((=='.') >>> bool 0 (1 / fromIntegral (length open)))
        & listArray rng

    simulate :: UArray (Int,Int) Double -> Int -> Double
    simulate chances i
        | exp < 0.000001 = exp
        | otherwise      = exp + simulate next (i+1)
      where
        exp = fromIntegral i * sum (map (next!) trap)
        next = runSTUArray $ do
            ret <- newArray rng 0
            forM_ open $ \((u,vs),f) -> forM_ vs $ \v -> modifyArray ret v (+(chances!u * f))
            pure ret

modifyArray :: (MArray a b m, Ix i) => a i b -> i -> (b -> b) -> m ()
modifyArray arr ix f = readArray arr ix >>= (f >>> writeArray arr ix)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
