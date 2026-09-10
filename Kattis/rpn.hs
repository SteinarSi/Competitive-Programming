{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> solve []
        >>> map show
        >>> unlines
        >>> putStr
    )

solve :: [Integer] -> [C.ByteString] -> [Integer]
solve _           ("quit":_)   = []
solve (a:stack)   ("dup":xs)   = solve (a:a:stack) xs
solve (s:stack)   ("print":xs) = s : solve (s:stack) xs
solve (_:stack)   ("pop":xs)   = solve stack xs
solve (b:a:stack) ("swap":xs)  = solve (a:b:stack) xs
solve (b:a:stack) ("+":xs)     = solve (a+b:stack) xs
solve (b:a:stack) ("-":xs)     = solve (a-b:stack) xs
solve (b:a:stack) ("/":xs)     = solve (a`div`b:stack) xs
solve (b:a:stack) ("*":xs)     = solve (a*b:stack) xs
solve (b:a:stack) ("^":xs)     = solve (a^b:stack) xs
solve stack       (x:xs)       = solve (readInteger x : stack) xs

readInteger :: C.ByteString -> Integer
readInteger = C.readInteger >>> fromJust >>> fst
