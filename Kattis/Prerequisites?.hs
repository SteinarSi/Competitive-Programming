{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Control.Monad         (replicateM, when)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.IntSet           (fromList, intersection, size)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    m <- C.getLine <&> (C.words >>> last >>> readInt)
    when (m /= 0) $ do
        courses <- C.getLine <&> (C.words >>> map readInt >>> fromList)
        categories <- replicateM m C.getLine <&> map (C.words >>> map readInt >>> (\(_:r:xs) -> (r, fromList xs)))

        C.putStrLn $ if all (\(r,xs) -> size (intersection courses xs) >= r) categories
            then "yes"
            else "no"

        main

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>>  fst
