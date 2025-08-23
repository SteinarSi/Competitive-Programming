{-# LANGUAGE FlexibleContexts #-}

import           Control.Arrow         ((>>>))
import           Control.Monad         (forM, forM_)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (UArray, listArray, (!))
import           Data.Array.ST         (STUArray, newArray, readArray,
                                        writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:xs <- C.getContents <&> (C.words >>> map (C.readInt >>> fromJust >>> fst))
    let perm = listArray (1,n) xs :: UArray Int Int
    print $ runST $ do
        done <- newArray (1,n) False :: ST s (STUArray s Int Bool)
        ret <- forM [1..n] $ \u -> do
            d <- readArray done u
            if d || perm ! u == u
                then pure 0
                else do
                    let cycle = u : takeWhile (u/=) (iterate (perm!) (perm!u))
                    forM_ cycle (flip (writeArray done) True)
                    pure (length cycle + 1)
        pure (sum ret)
