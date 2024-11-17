{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow            ((>>>), (&&&))
import           Control.Monad            (forM_, forM)
import           Control.Monad.ST         (ST, runST)
import           Data.Array               (Array, Ix)
import           Data.Array.Base          (UArray, MArray, writeArray, readArray, newArray, (!))
import           Data.Array.ST            (STArray, STUArray, freeze)
import qualified Data.ByteString.Char8 as C
import           Data.Function            ((&))
import           Data.Functor             ((<&>))
import           Data.Maybe               (fromJust, catMaybes)
import qualified Data.Sequence         as Seq
import           Data.Sequence            (Seq(..))

main :: IO ()
main = do
    ([c,p,x,l], pairs) <- C.getContents <&> (
                C.lines
            >>> map (C.words >>> map readInt)
            >>> head &&& (tail >>> map (head &&& last))
        )

    let leave = runST $ do
            (graph, total, curr) <- buildGraph c pairs
            out <- newArray (1,c) False
            writeArray out l True
            brexit graph out total curr (Seq.singleton l)
            readArray out x

    C.putStrLn $ if leave
        then "leave"
        else "stay"

brexit :: Array Int [Int] -> STUArray s Int Bool -> UArray Int Int -> STUArray s Int Int -> Seq Int -> ST s ()
brexit graph out total curr Empty = pure ()
brexit graph out total curr (x :<| xs) = forM (graph ! x) (\y -> do
            c <- readArray curr y
            writeArray curr y (c-1)
            o <- readArray out y
            if not o && (c-1) <= (total ! y) `div` 2
                then writeArray out y True >> pure (Just y)
                else pure Nothing
        ) >>= (catMaybes
            >>> Seq.fromList
            >>> (xs <>)
            >>> brexit graph out total curr
        )

buildGraph :: forall s. Int -> [(Int,Int)] -> ST s (Array Int [Int], UArray Int Int, STUArray s Int Int)
buildGraph c pairs = do
    graph <- newArray (1,c) [] :: ST s (STArray s Int [Int])
    curr  <- newArray (1,c) 0

    forM_ pairs $ \(a,b) -> do
        modifyArray curr a succ
        modifyArray curr b succ
        modifyArray graph a (b:)
        modifyArray graph b (a:)

    g <- freeze graph
    total <- freeze curr
    pure (g, total, curr)

modifyArray :: (MArray a b m, Ix i) => a i b -> i -> (b -> b) -> m ()
modifyArray arr ix f = readArray arr ix >>= (f >>> writeArray arr ix)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
