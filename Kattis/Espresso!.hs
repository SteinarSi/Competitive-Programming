{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n,s] <- C.getLine <&> (C.words >>> map (C.readInt >>> fromJust >>> fst))
    orders <- C.getContents <&> (C.lines >>> map readOrder)
    print (serve s s orders)

serve :: Int -> Int -> [Int] -> Int
serve s c [] = 0
serve s c (x:xs) | x > c = 1 + serve s s (x:xs)
                 | otherwise = serve s (c-x) xs

readOrder :: C.ByteString -> Int
readOrder x = case C.readInt x of
    Just (s,"")  -> s
    Just (s,"L") -> s+1
    _            -> error "bruh"
