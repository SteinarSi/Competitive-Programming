{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (bounds, listArray, newArray, readArray,
                                        writeArray, (!))
import           Data.Array.ST         (STUArray)
import           Data.Array.Unboxed    (UArray)
import qualified Data.ByteString.Char8 as C
import           Data.Char             (isAlpha)
import           Data.Functor          ((<&>))
import           Data.Ix               (inRange)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [r,c] <- C.getLine <&> (C.words >>> map readInt)
    graph <- C.getContents <&> (C.filter isAlpha
            >>> C.unpack
            >>> listArray ((1,1),(r,c))
        )
    let result = runST (newArray ((1,1),(r,c)) False >>= follow 0 graph (1,1))
    C.putStrLn result

follow :: Int -> UArray (Int,Int) Char -> (Int,Int) -> STUArray s (Int,Int) Bool -> ST s C.ByteString
follow steps graph pos@(y,x) seen
    | not (inRange (bounds graph) pos) = pure "Out"
    | otherwise = do
        s <- readArray seen pos
        if  | s                  -> pure "Lost"
            | graph ! pos == 'T' -> pure (C.pack (show steps))
            | otherwise          -> do
                writeArray seen pos True
                let next = case graph ! pos of
                        'N' -> (y-1,x)
                        'S' -> (y+1,x)
                        'W' -> (y,x-1)
                        'E' -> (y,x+1)
                follow (steps+1) graph next seen

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
