{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Control.Monad         (replicateM)
import qualified Data.ByteString.Char8 as C
import           Data.List             (delete)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n <- fmap readInt C.getLine
    names <- replicateM n C.getLine
    c <- fmap readInt C.getLine
    events <- fmap (map C.words) (replicateM c C.getLine)
    C.putStr . C.unlines $ process names events

process :: [C.ByteString] -> [[C.ByteString]] -> [C.ByteString]
process names [] = names
process names (e:events) = case e of
    ["leave", a]  -> process (delete a names) events
    ["cut", a, b] -> process (cut names a b) events

cut :: [C.ByteString] -> C.ByteString -> C.ByteString -> [C.ByteString]
cut [] _ _ = []
cut (n:names) a b | a == n = cut names a b
                  | b == n = a : b : cut names a b
                  | otherwise = n : cut names a b

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
