{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>), first)
import           Control.Monad         (filterM)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (STUArray, newArray, newArray_, runSTUArray, writeArray)
import           Data.Array.Base       (UArray, (!), readArray)
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    ncsf:ss:fs:ts' <- C.getContents <&> C.lines
    let [n,c,s,f] = C.words ncsf
            & map readInt
        sigma = C.unpack ss
        ts = map (C.words >>> map readInt >>> zip sigma) ts'
        table = runSTUArray $ do
            arr <- newArray_ ((1,'a'),(n,'z'))
            sequence_ [writeArray arr (i,c) j | (i,ts) <- zip [1..] ts, (c,j) <- ts]
            pure arr
        accepting = runSTUArray $ do
            accept <- newArray (1,n) False
            C.words fs
                & map readInt
                & mapM_ (flip (writeArray accept) True)
            pure accept

    print $ runST $ do
        seen <- newArray (1,n) False
        bfs seen table accepting ss [s]

bfs :: STUArray s Int Bool -> UArray (Int,Char) Int -> UArray Int Bool -> C.ByteString -> [Int] -> ST s Int
bfs seen table accepting sigma [] = error "bruh"
bfs seen table accepting sigma xs 
    | any (accepting!) xs = pure 0
    | otherwise = concatMap (\x -> C.foldr (\c -> (table ! (x,c) :)) [] sigma) xs
                & filterM (\y -> readArray seen y >>= \s -> if s then pure False else writeArray seen y True >> pure True)
                >>= bfs seen table accepting sigma 
                <&> succ

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
