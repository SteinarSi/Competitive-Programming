{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Control.Monad         (forM_)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.List             (sortOn)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n <- C.getLine <&> readInt
    forM_ [1..n] $ \i -> do
        (red, blu) <- C.getLine >> C.getLine <&> (C.words >>> parse [] [])
        C.putStr ("Case #" <> C.pack (show i) <> ": ")
        print (sum (red ++ blu) - length (red ++ blu))

parse :: [Int] -> [Int] -> [C.ByteString] -> ([Int], [Int])
parse red blu [] | length red > length blu = (take (length b) r, b)
                 | otherwise               = (r, take (length r) b)
    where r = sortOn negate red
          b = sortOn negate blu
parse red blu (x:xs) | C.last x == 'B' = parse red (readInt x : blu) xs
                     | otherwise       = parse (readInt x : red) blu xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
