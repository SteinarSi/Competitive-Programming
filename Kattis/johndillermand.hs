{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray, UArray, assocs, bounds,
                                        freeze, listArray, newArray, writeArray,
                                        (!))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (isSpace)
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Ix               (inRange)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [h,w] <- C.getLine <&> (C.words >>> map readInt)
    frame <- C.getContents <&> (C.filter (isSpace >>> not) >>> C.unpack >>> listArray ((1,1),(h,w)))

    let remove = runST $ do
            arr <- newArray ((1,1),(h,w)) False
            writeArray arr (1,1) True
            dillermand frame arr (1,1) (1,1)

    C.putStrLn (format w frame remove)

dillermand :: UArray (Int,Int) Char -> STUArray s (Int,Int) Bool -> (Int,Int) -> (Int,Int) -> ST s (UArray (Int,Int) Bool)
dillermand frame removed prev curr@(y,x) = case next of
        []  -> freeze removed
        [n] -> writeArray removed n True >> dillermand frame removed curr n
        _   -> error "bruh"
    where
        next = [(y+1,x),(y-1,x),(y,x+1),(y,x-1)]
                & filter (inRange (bounds frame))
                & filter ((frame !) >>> (`C.elem` "#O"))
                & filter (/= prev)
                & filter (/= curr)

format :: Int -> UArray (Int,Int) Char -> UArray (Int,Int) Bool -> C.ByteString
format w frame removed = assocs frame
        & map pixel
        & chunksOf w
        & map C.pack
        & C.intercalate "\n"
    where
        pixel (_,'.') = '.'
        pixel (p,c) | removed ! p = '.'
                    | otherwise   = c

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = let (a,b) = splitAt k xs
                in  a : chunksOf k b

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
