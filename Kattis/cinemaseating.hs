import           Control.Arrow         ((&&&), (>>>))
import           Control.Monad         (filterM, forM_)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (MArray, getElems, readArray)
import           Data.Array.ST         (Ix, STUArray, inRange, newArray, range,
                                        runSTUArray, writeArray)
import           Data.Array.Unboxed    (UArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    (rng,seats) <- C.getContents <&> parse

    runST $ do
        count <- newArray (0,8) 0 :: ST s (STUArray s Int Int)
        range rng
            & filter (seats !)
            & mapM_ (\(y,x) -> [(y-1,x-1),(y-1,x),(y-1,x+1),(y,x+1),(y+1,x+1),(y+1,x),(y+1,x-1),(y,x-1)]
                & filter (inRange rng)
                & filter (seats !)
                & length
                & flip (modifyArray count) succ
            )
        getElems count <&> (map (show >>> C.pack) >>> C.unwords >>> C.putStrLn)

parse :: C.ByteString -> (((Int,Int),(Int,Int)), UArray (Int,Int) Bool)
parse input = let (r,c):(n,_):xs = C.lines input
                        & map (C.words >>> map readInt >>> head &&& last)
                  rng = ((1,1),(r,c))
                  seats = runSTUArray $ do
                        arr <- newArray rng False
                        forM_ xs (flip (writeArray arr) True)
                        pure arr
              in  (rng, seats)

modifyArray :: (MArray a b m, Ix i) => a i b -> i -> (b -> b) -> m ()
modifyArray arr ix f = readArray arr ix >>= (f >>> writeArray arr ix)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
