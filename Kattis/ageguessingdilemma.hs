{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)
import           System.IO             (hFlush, stdout)

main :: IO ()
main = C.getLine >>= (readInt >>> (`solve` (1,10^9)))

solve :: Int -> (Int,Int) -> IO ()
solve b (lo,hi)
    | lo >= hi  = print lo
    | otherwise = do
        let g = lo + (hi+1-lo) `div` (b+1)
        print g
        hFlush stdout
        response <- C.getLine
        case response of
            "higher"  -> solve b (g+1,hi)
            "correct" -> pure ()
            "lower"   -> solve b (lo,g-1)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
