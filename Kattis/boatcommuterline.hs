{-# LANGUAGE MultiWayIf #-}

import           Control.Arrow         ((>>>))
import           Control.Monad         (forM_)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (STUArray, getAssocs, getElems,
                                        modifyArray, newArray, readArray,
                                        writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n,m,_]:xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    putStrLn $ runST $ do
        total <- newArray (1,m) 0 :: ST s (STUArray s Int Int)
        start <- newArray (1,m) (-1) :: ST s (STUArray s Int Int)

        forM_ xs $ \[p,c] -> do
            s <- readArray start c
            if | s == -1   -> writeArray start c p
               | s == p    -> writeArray start c (-1) >> modifyArray total c (100+)
               | otherwise -> writeArray start c (-1) >> modifyArray total c (abs (s-p) +)

        getAssocs start >>= (filter (snd >>> (/= -1)) >>> mapM_ (fst >>> flip (modifyArray total) (100+)))
        getElems total <&> (map show >>> unwords)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
