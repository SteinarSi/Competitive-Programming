{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((&&&), (>>>))
import           Control.Monad         (forM_)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray, getAssocs, newArray,
                                        readArray, writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust, isNothing)


main :: IO ()
main = do
    (n,m):xs <- C.getContents <&> (C.lines
            >>> map (C.words >>> map readInt >>> (head &&& last))
        )

    let degrees = runST $ do
            inn <- newArray (1,n) 0 :: ST s (STUArray s Int Int)
            out <- newArray (1,n) 0 :: ST s (STUArray s Int Int)
            forM_ xs $ \(i,j) -> do
                y <- readArray inn j
                writeArray inn j (y+1)
                x <- readArray out i
                writeArray out i (x+1)
            getAssocs inn >>= mapM (\(i,x) -> readArray out i <&> (\y -> (i,(x,y))))

    degrees
        & solve Nothing Nothing
        & C.putStrLn

solve :: Maybe Int -> Maybe Int -> [(Int, (Int,Int))] -> C.ByteString
solve a b [] = case (a,b) of
    (Nothing, Nothing) -> "anywhere"
    (Just x , Just y ) -> C.pack (show x) <> " " <> C.pack (show y)
    _                  -> "no"
solve a b ((i,(inn,out)):xs) = case out - inn of
    1  | isNothing a -> solve (Just i) b xs
       | otherwise -> "no"
    -1 | isNothing b -> solve a (Just i) xs
       | otherwise -> "no"
    0 -> solve a b xs
    _ -> "no"

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
