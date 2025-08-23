import           Control.Arrow         ((>>>))
import           Control.Monad         (filterM, unless)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray, UArray, assocs, bounds,
                                        getAssocs, listArray, newArray,
                                        readArray, writeArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Ix               (inRange)
import           Data.List             (find)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    r':c':xs <- C.getContents <&> C.words

    let r = readInt r'
        c = readInt c'
        grid = listArray rng (concatMap C.unpack xs)
        rng = ((1,1),(r,c))
        s = assocs grid
            & find (snd >>> (=='S'))
            & fromJust
            & fst
        fs = assocs grid
            & filter (snd >>> (=='*'))
            & map fst

    putStrLn $ runST $ do
            seen <- newArray rng False
            flooded <- newArray rng False
            mapM_ (flip (writeArray flooded) True) fs
            escape grid seen flooded [s] fs <&> maybe "KAKTUS" show

escape :: UArray (Int,Int) Char -> STUArray s (Int,Int) Bool -> STUArray s (Int,Int) Bool -> [(Int,Int)] -> [(Int,Int)] -> ST s (Maybe Int)
escape _ _ _ [] _              = pure Nothing
escape grid seen flooded qs fs | any ((grid!) >>> (=='D')) qs = pure (Just 0)
                               | otherwise = do
    fs' <- concatMap (neighbours >>> filter ((grid!) >>> (`notElem` "DX"))) fs
        & filterM (\v -> readArray flooded v >>= \f -> unless f (writeArray flooded v True) >> pure (not f))
    qs' <- concatMap (neighbours >>> filter ((grid!) >>> (/='X'))) qs
        & filterM (\v -> do
            f <- readArray flooded v
            s <- readArray seen v
            unless (f || s) $ do
                writeArray flooded v True
                writeArray seen v True
            pure (not f && not s))
    escape grid seen flooded qs' fs' <&> (<&> succ)
  where
    neighbours :: (Int,Int) -> [(Int,Int)]
    neighbours (x,y) = filter (inRange (bounds grid)) [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
