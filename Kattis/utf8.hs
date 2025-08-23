{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> count (0,0,0,0)
        >>> C.putStrLn
    )

count :: (Int,Int,Int,Int) -> [C.ByteString] -> C.ByteString
count (a,b,c,d) [] = C.init . C.unlines $ map (show >>> C.pack) [a,b,c,d]
count (a,b,c,d) ys = case ys of
    (x:xs) | C.isPrefixOf "0" x -> count (a+1,b,c,d) xs
    (x:y:xs) | C.isPrefixOf "110" x && C.isPrefixOf "10" y -> count (a,b+1,c,d) xs
    (x:y:z:xs) | C.isPrefixOf "1110" x && C.isPrefixOf "10" y && C.isPrefixOf "10" z -> count (a,b,c+1,d) xs
    (x:y:z:u:xs) | C.isPrefixOf "11110" x && C.isPrefixOf "10" y && C.isPrefixOf "10" z && C.isPrefixOf "10" u -> count (a,b,c,d+1) xs
    _ -> "invalid"
