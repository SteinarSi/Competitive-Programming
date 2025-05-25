{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import qualified Data.Set              as S

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.head &&& C.drop 2)
        >>> solve S.empty
        >>> C.unlines
        >>> C.putStr
    )

solve :: S.Set C.ByteString -> [(Char,C.ByteString)] -> [C.ByteString]
solve guests [] = []
solve guests (('+',x):xs) = solve (S.insert x guests) xs
solve guests (('-',x):xs) = solve (S.delete x guests) xs
solve guests (('?',x):xs) | S.member x guests = "Jebb"  : solve guests xs
                          | otherwise         = "Neibb" : solve guests xs
