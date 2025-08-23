import           Control.Arrow         (second, (>>>))
import           Control.Monad         (filterM, when)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (MArray (..), UArray, bounds, elems,
                                        freeze, listArray, readArray,
                                        writeArray, (!))
import           Data.Array.ST         (Ix (..), STArray, STUArray)
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)
import           Data.Sequence         (Seq (..))
import qualified Data.Sequence         as Seq

main :: IO ()
main = do
    r:c:d:y:x:xs <- C.getContents <&> (C.words >>> map readInt)

    listArray ((1,1),(r,c)) xs
        & solve d (y,x)
        & elems
        & chunksOf c
        & map (map show >>> unwords)
        & unlines
        & putStr

solve :: Int -> (Int,Int) -> UArray (Int,Int) Int -> UArray (Int,Int) Int
solve m s grid = runST $ do
    ret <- newArray (bounds grid) 0
    queues <- newArray (0,20) Empty
    writeArray ret s m
    modifyArray queues (grid!s) (s:<|)
    solve' ret queues 20
  where
    solve' :: STUArray s (Int,Int) Int -> STArray s Int (Seq (Int,Int)) -> Int -> ST s (UArray (Int,Int) Int)
    solve' ret _ (-1)       = freeze ret
    solve' ret queues level = do
        queue <- readArray queues level
        case queue of
            Empty -> solve' ret queues (pred level)
            (u@(y,x) :<| xs) -> do
                modifyArray queues level (Seq.drop 1)
                d <- readArray ret u
                when (d > 1) $ do
                    [(y-1,x),(y+1,x),(y,x-1),(y,x+1)]
                        & filter (inRange (bounds grid))
                        & filter ((grid!) >>> (<=(grid!u)))
                        & filterM (readArray ret >>> (<&> (<d-1)))
                        >>= mapM_ (\v -> do
                            writeArray ret v (bool (d-1) m (grid!v < grid!u))
                            modifyArray queues (grid!v) (:|>v)
                        )
                solve' ret queues level

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = splitAt k xs
    & second (chunksOf k)
    & uncurry (:)

modifyArray :: (MArray a b m, Ix i) => a i b -> i -> (b -> b) -> m ()
modifyArray arr ix f = readArray arr ix >>= (f >>> writeArray arr ix)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
