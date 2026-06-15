{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Control.Monad         (filterM, unless)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray, UArray, freeze, readArray,
                                        writeArray, (!))
import           Data.Array.ST         (inRange, newArray)
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    rc:xs <- C.getContents <&> (C.lines >>> map C.words)
    let [r,c,_] = map readInt rc
        rng = ((0,0),(r-1,c-1))
    putStrLn $ runST $ do
        east <- newArray rng maxBound :: ST s (STUArray s (Int,Int) Int)
        south <- newArray rng maxBound :: ST s (STUArray s (Int,Int) Int)
        mapM_ (\[y,x,k,t] -> writeArray (bool east south (k=="S")) (readInt y, readInt x) (readInt t)) xs
        e <- freeze east
        s <- freeze south
        seen <- newArray ((0,0),(r-1,c-1)) False
        solve rng (e,s) seen [(0,(0,0))] <&> maybe "March the 30th be with you!" show

solve :: ((Int,Int),(Int,Int)) -> (UArray (Int,Int) Int,UArray (Int,Int) Int) -> STUArray s (Int,Int) Bool -> [(Int,(Int,Int))] -> ST s (Maybe Int)
solve rng (east,south) seen [] = pure Nothing
solve rng (east,south) seen ((t,(y,x)):xs)
    | (y,x) == snd rng = pure (Just t)
    | otherwise = [(south ! (y-1,x), (y-1,x)), (south ! (y,x), (y+1,x)), (east ! (y,x-1), (y,x-1)), (east ! (y,x), (y,x+1))]
        & filter (snd >>> inRange rng)
        & filter (fst >>> (>t))
        & map snd
        & filterM (\v -> readArray seen v >>= \s -> unless s (writeArray seen v True) >> pure (not s))
        >>= (map (t+1,) >>> (xs<>) >>> solve rng (east,south) seen)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
