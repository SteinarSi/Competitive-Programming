{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray, newArray, readArray,
                                        writeArray)
import           Data.Array.Unboxed    (UArray, bounds, elems, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (chr, ord)
import           Data.Function         (on, (&))
import           Data.Functor          ((<&>))
import           Data.Ix               (inRange)
import           Data.List             (sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:t:xs <- C.getContents <&> (C.words >>> map readInt)
    let a = listArray (0,n-1) xs
    C.putStrLn $ case t of
        1 -> "7"
        2 -> case on compare (a!) 0 1 of
            LT -> "Smaller"
            EQ -> "Equal"
            GT -> "Bigger"
        3 -> map (a!) [0..2]
            & sort
            & (!! 1)
            & show
            & C.pack
        4 -> elems a
            & sum
            & show
            & C.pack
        5 -> elems a
            & filter even
            & sum
            & show
            & C.pack
        6 -> elems a
            & map ((`mod`26) >>> (+ord 'a') >>> chr)
            & C.pack
        7 -> runST (newArray (0,n-1) False >>= search a 0)

search :: UArray Int Int -> Int -> STUArray s Int Bool -> ST s C.ByteString
search a i seen = do
    s <- readArray seen i
    if
        | i == snd (bounds a)        -> pure "Done"
        | s                          -> pure "Cyclic"
        | not (bounds a `inRange` j) -> pure "Out"
        | otherwise                  -> writeArray seen i True >> search a j seen
    where j = a ! i

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
