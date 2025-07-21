{-# LANGUAGE MultiWayIf #-}

import           Control.Arrow         ((&&&), (***), (>>>))
import           Control.Monad         (filterM)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (STUArray, inRange, newArray, readArray,
                                        writeArray)
import           Data.Array.Unboxed    (UArray, bounds, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)
import qualified Data.Set              as S

main :: IO ()
main = do
    r:c:xs <- C.getContents <&> (C.words >>> map readInt)

    let rng = ((1,1),(r,c))
        grid = listArray rng xs

    print $ runST $ do
        seen <- newArray rng False
        [1..r]
            & map ((,1) >>> ((grid!) &&& id))
            & S.fromList
            & search grid seen

search :: UArray (Int,Int) Int -> STUArray s (Int,Int) Bool -> S.Set (Int,(Int,Int)) -> ST s Int
search grid seen queue = do
    s <- readArray seen (y,x)
    if | not (inRange (bounds grid) (y,x+1)) -> pure c
       | s -> search grid seen queue'
       | otherwise -> do
            writeArray seen (y,x) True
            [(y,x+1),(y+1,x),(y,x-1),(y-1,x)]
                & filter (inRange (bounds grid))
                & filterM (readArray seen >>> (<&> not))
                >>= (map (((grid!) >>> max c) &&& id) >>> foldr S.insert queue' >>> search grid seen)
  where
    ((c,(y,x)),queue') = S.deleteFindMin queue

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
