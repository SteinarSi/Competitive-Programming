{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Control.Monad         (filterM, forM_)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray, getAssocs, newArray,
                                        unsafeRead, unsafeWrite, (!))
import           Data.Array.ST         (runSTArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.IntSet           as S
import           Data.List             (partition)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n <- C.getLine <&> readInt
    xs <- C.getContents <&> (C.lines >>> map (C.words >>> \[a,x,b] -> (x,(readInt a, readInt b))))

    let (known,unknown) = partition (fst >>> (==">")) xs

        graph = runSTArray $ do
            ret <- newArray (0,n-1) []
            forM_ unknown $ \(_,(a,b)) -> do
                unsafeRead ret a >>= ((b:) >>> unsafeWrite ret a)
                unsafeRead ret b >>= ((a:) >>> unsafeWrite ret b)
            pure ret

        eliminate :: STUArray s Int Bool -> Int -> Int -> ST s ()
        eliminate isChild parent child = graph ! child
            & filter (/=parent)
            & filterM (\v -> unsafeRead isChild v >>= \c -> if c then pure False else unsafeWrite isChild v True >> pure True)
            >>= mapM_ (eliminate isChild child)

    putStrLn $ runST $ do
        isChild <- newArray (0,n-1) False
        mapM_ (\(_,(_,b)) -> unsafeWrite isChild b True) known
        mapM_ (\(_,(a,b)) -> eliminate isChild a b) known
        getAssocs isChild <&> (filter (snd >>> not) >>> map (fst >>> show) >>> unwords)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
