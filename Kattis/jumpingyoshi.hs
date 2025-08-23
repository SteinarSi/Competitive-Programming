import           Control.Arrow         ((>>>))
import           Control.Monad         (unless, when)
import           Control.Monad.ST      (ST, runST)
import           Data.Array            (Array, Ix (..))
import           Data.Array.Base       (MArray (..), UArray, bounds, getAssocs,
                                        listArray, readArray, writeArray, (!))
import           Data.Array.ST         (STUArray, runSTArray)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:xs <- C.getContents <&> (C.words >>> map readInt)

    let pebbles = listArray (0,n-1) xs
        src = build (+) n xs
        trg = build (-) n xs

    print (runST (newArray (0,n-1) False >>= solve pebbles src trg))

solve :: forall s. UArray Int Int -> Array Int [Int] -> Array Int [Int] -> STUArray s Int Bool -> ST s Int
solve pebbles src trg seen = jump 0 >> getAssocs seen <&> (filter snd >>> map fst >>> last)
    where
        jump :: Int -> ST s ()
        jump i = readArray seen i >>= flip unless (do
                writeArray seen i True
                when (inRange (bounds pebbles) (i + pebbles ! i)) (mapM_ jump (trg ! (i + pebbles ! i)))
                when (inRange (bounds pebbles) (i - pebbles ! i)) (mapM_ jump (src ! (i - pebbles ! i))))

build :: (Int -> Int -> Int) -> Int -> [Int] -> Array Int [Int]
build f n xs = runSTArray $ do
    arr <- newArray (0,n-1) []
    sequence_ [ modifyArray arr (f i x) (i:) | (i,x) <- zip [0..] xs, inRange (0,n-1) (f i x)]
    pure arr

modifyArray :: (MArray a b m, Ix i) => a i b -> i -> (b -> b) -> m ()
modifyArray arr ix f = readArray arr ix >>= (f >>> writeArray arr ix)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
