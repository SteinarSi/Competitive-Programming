{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (STUArray, newArray, readArray,
                                        writeArray)
import           Data.Array.Unboxed    (UArray, assocs, listArray, (!))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Ix               (inRange)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n <- C.getLine <&> readInt
    board <- C.getLine <&> (C.unpack >>> listArray ((1,1),(n,n)))

    putStrLn $ runST $ do
        dp <- newArray (((1,1),1),((n,n),C.length goal - 1)) False
        assocs board
            & filter (snd >>> (=='I'))
            & mapM (fmap (const 1) >>> solve n board dp)
            <&> (or >>> bool "NO" "YES")

solve :: Int -> UArray (Int,Int) Char -> STUArray s ((Int,Int),Int) Bool -> ((Int,Int),Int) -> ST s Bool
solve n board dp ((x,y),i) | i >= C.length goal = pure True
                           | otherwise = do
    r <- readArray dp ((x,y),i)
    writeArray dp ((x,y),i) True
    if r
        then pure False
        else [(x+1,y+2),(x+2,y+1),(x+2,y-1),(x+1,y-2),(x-1,y-2),(x-2,y-1),(x-2,y+1),(x-1,y+2)]
                & filter (inRange ((1,1),(n,n)))
                & filter ((board!) >>> (==C.index goal i))
                & mapM (\v -> solve n board dp (v,i+1))
                <&> or

goal :: C.ByteString
goal = "ICPCASIASG"

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
