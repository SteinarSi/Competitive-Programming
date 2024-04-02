{-# LANGUAGE FlexibleContexts #-}

import           Control.Arrow         ((>>>))
import           Control.Monad         (filterM, forM_, replicateM, unless)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (IArray (..), MArray (..), STUArray,
                                        UArray, assocs, indices, listArray,
                                        readArray, writeArray, (!))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Ix               (Ix (..))
import           Data.Maybe            (fromJust)
import           System.IO             (isEOF)

main :: IO ()
main = loop 1

loop :: Int -> IO ()
loop i = do
    end <- isEOF
    unless end $ do
        [m,n] <- C.getLine <&> (C.words >>> map readInt)
        image <- replicateM m C.getLine <&> (C.concat
            >>> C.unpack
            >>> listArray ((1,1),(m,n))
            )
        C.putStrLn (C.pack (concat [
            "Case ",
            show i,
             ": ",
             show (runST (newArray (bounds image) False >>= solulu image))
            ]
            ))
        loop (i+1)

solulu :: UArray (Int,Int) Char -> STUArray s (Int,Int) Bool -> ST s Int
solulu image seen = do
    assocs image
        & filter (snd >>> ('-'==))
        & mapM (\(xy,_) -> readArray seen xy >>= bool (search xy >> pure 1) (pure 0))
        <&> sum
    where search (x,y) = do
            s <- readArray seen (x,y)
            unless s $ do
                writeArray seen (x,y) True
                [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
                    & filter (inRange (bounds image))
                    & filter ((image!) >>> ('-'==))
                    & filterM (readArray seen >>> fmap not)
                    >>= mapM_ search

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
