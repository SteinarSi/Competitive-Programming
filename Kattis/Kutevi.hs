{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Control.Monad         (filterM, forM_, unless)
import           Control.Monad.ST      (ST)
import           Data.Array.Base       (STUArray, newArray, readArray,
                                        writeArray, (!))
import           Data.Array.ST         (runSTUArray)
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.IntSet           as S
import           Data.Maybe            (fromJust)
import           Data.STRef            (STRef, modifySTRef', newSTRef,
                                        readSTRef)
import qualified Data.Vector.Mutable   as MV

main :: IO ()
main = do
    [n,m]:xs:ys:_ <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    let possible = runSTUArray $ do
            poss <- newArray (0,359) False
            angles <- MV.unsafeNew 360
            ix <- newSTRef 0
            anglify xs ix angles poss
            pure poss
    mapM_ ((possible!)
        >>> bool "NO" "YES"
        >>> C.putStrLn
        ) ys

anglify :: forall s. [Int] -> STRef s Int -> MV.STVector s Int -> STUArray s Int Bool -> ST s ()
anglify xs ix angles possible = forM_ xs anglify'
    where
        anglify' :: Int -> ST s ()
        anglify' x = do
            i <- readSTRef ix
            modifySTRef' ix succ
            MV.write angles i x
            writeArray possible x True
            ys <- mapM (MV.read angles) [0..i]
            forM_ ys $ \y -> mapM_ (
                        (`mod`360)
                    >>> (\z -> do
                        b <- readArray possible z
                        unless b (anglify' z)
                    )) [x+y,x-y,y-x]

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
