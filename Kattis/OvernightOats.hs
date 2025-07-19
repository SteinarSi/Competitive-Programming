{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)
import           Data.Sequence         (Seq (..), dropWhileL)

main :: IO ()
main = do
    _:x:xs <- C.getContents <&> C.lines
    putStrLn $ if solve (readInt x) Empty (zip [1..] xs)
        then "yay!"
        else "ono.."

solve :: Int -> Seq Int -> [(Int,C.ByteString)] -> Bool
solve _ _ []                 = True
solve l ps ((_,"PASS"):xs) = solve l ps xs
solve l ps ((i,"ADD"):xs) = solve l (ps :|> i) xs
solve l ps ((i,"EAT"):xs) = case dropWhileL ((i-) >>> (>l)) ps of
    Empty       -> False
    (p :<| ps') -> solve l ps' xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
