import           Control.Arrow         ((>>>))
import           Control.Monad         (filterM)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (STUArray, newArray, readArray,
                                        writeArray)
import           Data.Array.Unboxed    (UArray, inRange, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n,m]:xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    let
        rng = ((1,1),(n,m))

        grid :: UArray (Int,Int) Int
        grid = listArray rng (concat xs)

        [sx,sy,tx,ty] = last xs

        solve :: STUArray s ((Int,Int),Bool) Int -> Int -> [((Int,Int),Bool)] -> ST s ()
        solve dist _ [] = pure ()
        solve dist d xs = concatMap (\(u@(y,x),c) -> [(y-1,x),(y-1,x+1),(y,x+1),(y+1,x+1),(y+1,x),(y+1,x-1),(y,x-1),(y-1,x-1)]
            & filter (inRange rng)
            & filter (\v -> grid ! v /= grid ! u && (grid ! v < grid ! u) /= c)
            & map (,not c)) xs
            & filterM (\v -> readArray dist v >>= \dv -> if d < dv then writeArray dist v d >> pure True else pure False)
            >>= solve dist (d+1)

    print $ runST $ do
        dist <- newArray ((fst rng,False),(snd rng,True)) maxBound
        writeArray dist ((sx,sy),False) 0
        writeArray dist ((sx,sy),True) 0
        solve dist 1 [((sx,sy),False),((sx,sy),True)]
        d1 <- readArray dist ((tx,ty),False)
        d2 <- readArray dist ((tx,ty),True)
        let d = min d1 d2
        pure $ if d < maxBound
            then d
            else -1

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
