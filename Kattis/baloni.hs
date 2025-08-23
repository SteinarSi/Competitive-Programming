import           Control.Arrow         ((>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (Ix, MArray, STUArray, newArray,
                                        readArray, writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    _:xs <- C.getContents <&> (C.words >>> map readInt)

    print $ runST $ do
        pops <- newArray (0,1000000) 0
        solve pops 0 xs

solve :: STUArray s Int Int -> Int -> [Int] -> ST s Int
solve pops r [] = pure r
solve pops r (x:xs) = do
    s <- readArray pops x
    if s > 0
        then do
            modifyArray pops x     pred
            modifyArray pops (x-1) succ
            solve pops r xs
        else do
            modifyArray pops (x-1) succ
            solve pops (r+1) xs

modifyArray :: (MArray a b m, Ix i) => a i b -> i -> (b -> b) -> m ()
modifyArray arr ix f = readArray arr ix >>= (f >>> writeArray arr ix)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
