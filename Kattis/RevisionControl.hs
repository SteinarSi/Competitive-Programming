{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Control.Monad         (forM)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (STUArray, newArray, readArray,
                                        writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> tail
        >>> map (C.readInt >>> fromJust >>> fst)
        >>> solve
        >>> C.unwords
        >>> C.putStrLn
    )

solve :: [Int] -> [C.ByteString]
solve xs = runST $ do
    seen <- newArray (1,10^9) False :: ST s (STUArray s Int Bool)
    forM xs $ \x -> do
        s <- readArray seen x
        if s
            then pure "0"
            else writeArray seen x True >> pure "1"
