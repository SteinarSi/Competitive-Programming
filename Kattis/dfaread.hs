{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>), first)
import           Data.Array.ST         (STUArray, newArray, newArray_, runSTUArray, writeArray)
import           Data.Array.Base       (UArray, (!))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    ncsf:ss:fs:ls <- C.getContents <&> C.lines
    let [n,c,s,f] = C.words ncsf
            & map readInt
        sigma = C.unpack ss
        (tss,_:queries) = splitAt n ls
            & first (map (C.words >>> map readInt >>> zip sigma))
        table = runSTUArray $ do
            arr <- newArray_ ((1,'a'),(n,'z'))
            sequence_ [writeArray arr (i,c) j | (i,ts) <- zip [1..] tss, (c,j) <- ts]
            pure arr
        accepting = runSTUArray $ do
            accept <- newArray (1,n) False
            C.words fs
                & map readInt
                & mapM_ (flip (writeArray accept) True)
            pure accept

    queries
        & map (dfa s table accepting)
        & C.unlines
        & C.putStr

dfa :: Int -> UArray (Int,Char) Int -> UArray Int Bool -> C.ByteString -> C.ByteString
dfa s table accepting = C.foldl (curry (table!)) s 
        >>> (accepting !) 
        >>> bool "reject" "accept"

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
