import           Control.Arrow         ((>>>))
import           Control.Monad         (filterM)
import           Control.Monad.ST      (ST, runST)
import           Data.Array            (Array)
import           Data.Array.ST         (STUArray, newArray, newArray_, runSTUArray, writeArray, runSTArray)
import           Data.Array.Base       (MArray, UArray, amap, assocs, readArray, (!))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Ix               (Ix)
import           Data.Maybe            (fromJust)
import qualified Data.IntSet           as S

main :: IO ()
main = do
    ncsf:_:fs:rest <- C.getContents <&> C.lines
    let [n,c,s,f] = C.words ncsf
            & map readInt
        tss = map (C.words >>> map readInt >>> zip [1..]) rest
        table = runSTUArray $ do
            arr <- newArray_ ((1,1),(n,c))
            sequence_ [writeArray arr (i,c) j | (i,ts) <- zip [1..] tss, (c,j) <- ts]
            pure arr
        finals = C.words fs
            & map readInt
        reach = reachable n finals table

        infinite = runST $ do
            seen <- newArray (1,n) False
            stack <- newArray (1,n) False
            writeArray seen s True
            writeArray stack s True
            loop c table reach seen stack s

    putStrLn $ if infinite
        then "infinite"
        else "finite"

loop :: Int -> UArray (Int,Int) Int -> UArray Int Bool -> STUArray s Int Bool -> STUArray s Int Bool -> Int -> ST s Bool
loop c table reach seen stack u = do
    let neighbours = map ((u,) >>> (table!)) [1..c]
    done <- neighbours
        & filter (reach!)
        & anyM (readArray stack) 
    if done
        then pure True
        else neighbours
            & filterM (\x -> readArray seen x >>= bool (writeArray seen x True >> pure True) (pure False))
            >>= anyM (\v -> do
                writeArray stack v True
                r <- loop c table reach seen stack v
                writeArray stack v False
                pure r)

reachable :: Int -> [Int] -> UArray (Int,Int) Int -> UArray Int Bool
reachable n fs table = runSTUArray $ do
        seen <- newArray (1,n) False
        mapM_ (flip (writeArray seen) True) fs
        reach seen fs
    where
        inverse = amap S.toList $ runSTArray $ do
            arr <- newArray (1,n) S.empty
            mapM_ (\((i,c),j) -> modifyArray arr j (S.insert i)) (assocs table)
            pure arr

        reach :: STUArray s Int Bool -> [Int] -> ST s (STUArray s Int Bool)
        reach seen [] = pure seen
        reach seen xs = concatMap (inverse!) xs
                & filterM (\x -> readArray seen x >>= bool (writeArray seen x True >> pure True) (pure False))
                >>= reach seen

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM _ [] = pure False
anyM p (x:xs) = do
        b <- p x
        if b
            then pure True
            else anyM p xs

modifyArray :: (MArray a b m, Ix i) => a i b -> i -> (b -> b) -> m ()
modifyArray arr ix f = readArray arr ix >>= (f >>> writeArray arr ix)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
