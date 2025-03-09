import           Control.Arrow         ((&&&), (>>>))
import           Control.Monad         (zipWithM_)
import           Control.Monad.ST      (runST)
import           Data.Array            (Array)
import           Data.Array.Base       (MArray (newArray), UArray, listArray,
                                        readArray, writeArray, (!))
import           Data.Array.ST         (runSTArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Ix               (Ix)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    ((n,_):xs) <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt >>> head &&& last))

    let fun :: UArray Int Int
        fun = listArray (0,n-1) (map fst xs)

        graph :: Array Int [Int]
        graph = runSTArray $ do
            ret <- newArray (0,n-1) []
            drop 1 xs
                & map snd
                & zipWithM_ (\u v -> modifyArray ret v (u:)) [1..]
            pure ret

        either :: Array Int Int
        either = listArray (0,n-1) (map f [0..n-1])
            where
                f :: Int -> Int
                f x = graph ! x
                    & (map (skip!) >>> (fun!x:) >>> sum)
                        &&&
                      (map (either!) >>> sum)
                    & uncurry max

        skip :: Array Int Int
        skip = listArray (0,n-1) (map (\x -> sum (map (either!) (graph ! x))) [0..n-1])

    print (either ! 0)

parse :: [(Int,Int)] -> (Int, UArray Int Int, Array Int [Int])
parse ((n,_):xs) = (n, fun, graph)
  where
    fun = listArray (0,n-1) (map fst xs)
    graph = runSTArray $ do
        ret <- newArray (0,n-1) []
        drop 1 xs
            & map snd
            & zipWithM_ (\u v -> modifyArray ret v (u:)) [1..]
        pure ret

modifyArray :: (MArray a b m, Ix i) => a i b -> i -> (b -> b) -> m ()
modifyArray arr ix f = readArray arr ix >>= (f >>> writeArray arr ix)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
