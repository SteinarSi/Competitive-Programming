import           Control.Arrow         ((>>>))
import           Control.Monad         (filterM, liftM2)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (STUArray, newArray, readArray,
                                        writeArray)
import           Data.Array.Unboxed    (UArray, assocs, bounds, inRange,
                                        listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (digitToInt)
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (find)
import           Data.Maybe            (fromJust)
import qualified Data.Set              as S

main :: IO ()
main = do
    nm:xs <- C.getContents <&> C.lines

    let [n,m] = map readInt (C.words nm)
        rng = ((1,1),(n,m))
        grid = listArray rng (concatMap C.unpack xs)
        Just (s,_) = find (snd >>> (=='S')) (assocs grid)
        Just (e,_) = find (snd >>> (=='E')) (assocs grid)

    print $ runST $ do
        dist <- newArray (((1,1),False),((n,m),True)) maxBound
        search grid dist (S.singleton (0,s,True))
        d <- liftM2 min (readArray dist (e,True)) (readArray dist (e,False))

        pure $ if d < maxBound
            then d
            else -1

search :: UArray (Int,Int) Char -> STUArray s ((Int,Int),Bool) Int -> S.Set (Int,(Int,Int),Bool) -> ST s ()
search grid dist queue = case S.minView queue of
    Nothing               -> pure ()
    Just ((d,(y,x),j),q') -> do
        let ns = [(y,x-1),(y,x+1),(y-1,x),(y+1,x)]
                & filter (inRange (bounds grid))
                & filter ((grid!) >>> (/='#'))
            pick | j = map (d+1,,False) ns
                 | otherwise = []
            skip = map (\u -> (d+cost u, u, j)) ns

        (skip <> pick)
            & filterM (\(d,u,j) -> readArray dist (u,j) >>= \d' -> if d' > d then writeArray dist (u,j) d >> pure True else pure False)
            >>= (foldr S.insert q' >>> search grid dist)
  where
    cost (y,x) = case grid ! (y,x) of
        'S' -> 0
        'E' -> 0
        c   -> digitToInt c

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
