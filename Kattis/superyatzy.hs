{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as C
import Control.Arrow ((>>>))
import Data.Maybe (fromJust)
import Data.Functor ((<&>))
import Control.Monad.ST (runST, ST)
import Control.Monad (forM_, forM)
import Data.Array.ST (STUArray, newArray, getElems, writeArray, readArray)

main :: IO ()
main = do
    n:m:xs <- C.getContents <&> (C.words >>> map readInt)

    let highest = runST $ do
            count <- newArray (1,6) 0 :: ST s (STUArray s Int Int)
            forM_ xs (\x -> readArray count x >>= (succ >>> writeArray count x))
            getElems count <&> maximum

    C.putStrLn $ if highest + m >= n
        then "Ja"
        else "Nej"

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
