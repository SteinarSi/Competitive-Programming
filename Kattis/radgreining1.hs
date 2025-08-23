{-# LANGUAGE MultiWayIf #-}

import           Control.Arrow         ((>>>))
import           Control.Monad         (forM)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (getElems, newArray, readArray,
                                        writeArray)
import           Data.Array.ST         (STUArray)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n <- C.getLine <&> readInt
    sections <- C.getContents <&> (C.lines >>> map (C.words >>> (\(a:b:_) -> (readInt a, C.unpack b))))
    putStrLn $ runST (newArray (1,n) '?' >>= solve sections)

solve :: [(Int,String)] -> STUArray s Int Char -> ST s String
solve [] ret = getElems ret
solve ((i,xs):xss) ret = do
    r <- forM (zip [i..] xs) $ \(j,x) -> do
        y <- readArray ret j
        if
            | y == '?'  -> writeArray ret j x >> pure False
            | y == x    -> pure False
            | otherwise -> pure True
    if or r
        then pure "Villa"
        else solve xss ret

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
