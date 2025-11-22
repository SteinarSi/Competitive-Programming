{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> head &&& last)
        >>> solve ("alice","bob","charlie")
        >>> C.unlines
        >>> C.putStr
    )

solve :: (C.ByteString,C.ByteString,C.ByteString) -> [(C.ByteString,C.ByteString)] -> [C.ByteString]
solve (f,g,h) [] = []
solve (f,g,h) ((n,s):xs) = case s of
    "fishing" | n == f    -> n <> " already has fishing" : solve (f,g,h) xs
              | otherwise -> n <> " borrows fishing from " <> f : solve (n,g,h) xs
    "golf"    | n == g    -> n <> " already has golf" : solve (f,g,h) xs
              | otherwise -> n <> " borrows golf from " <> g : solve (f,n,h) xs
    "hockey"  | n == h    -> n <> " already has hockey" : solve (f,g,h) xs
              | otherwise -> n <> " borrows hockey from " <> h : solve (f,g,n) xs
