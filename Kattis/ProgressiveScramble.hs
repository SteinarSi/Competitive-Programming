{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow            ((>>>), first)
import qualified Data.ByteString.Char8 as C
import           Data.Bool                (bool)
import           Data.Char                (ord, chr)
import           Data.Function            ((&))

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> map (C.splitAt 2 
            >>> first ((=="e ") >>> bool decrypt encrypt) 
            >>> uncurry ($))
        >>> C.unlines
        >>> C.putStr
    )

decrypt :: C.ByteString -> C.ByteString
decrypt = C.unpack >>> dec 0 >>> C.pack
    where
        dec :: Int -> String -> String
        dec p "" = ""
        dec p (x:xs) = int2Char u : dec p' xs
            where
                u = (char2Int x - p) `mod` 27
                p' = u + p

encrypt :: C.ByteString -> C.ByteString
encrypt = C.unpack >>> enc 0 >>> C.pack
    where
        enc :: Int -> String -> String
        enc _ "" = ""
        enc p (x:xs) = int2Char u : enc p' xs
            where
                u = (p + char2Int x) `mod` 27
                p' | x == ' '  = p
                   | otherwise = u

char2Int :: Char -> Int
char2Int ' ' = 0
char2Int x = ord x - ord 'a' + 1

int2Char :: Int -> Char
int2Char 0 = ' '
int2Char x = chr (x + ord 'a' - 1)
