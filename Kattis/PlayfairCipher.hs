{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow            ((>>>), (&&&))
import qualified Data.ByteString.Char8 as C
import Data.Functor ((<&>))
import Data.Array.Base (UArray, array, (!))
import Data.List (nub)
import Data.Function ((&))
import Data.Ix (range)
import Data.Tuple (swap)
import Data.Char (toUpper)
import Data.Array (Array)

main :: IO ()
main = C.getContents >>= (
            C.filter (`C.notElem` "Q ")
        >>> C.map toUpper
        >>> C.lines
        >>> (head >>> createTables) 
                &&& 
            (last >>> C.unpack >>> digraphs)
        >>> uncurry encrypt
        >>> C.pack
        >>> C.putStrLn
    )

encrypt :: (UArray (Int,Int) Char, Array Char (Int,Int)) -> [(Char,Char)] -> String
encrypt (pos2char, char2pos) [] = ""
encrypt (pos2char, char2pos) ((a,b):xs) = pos2char ! ixA : pos2char ! ixB : encrypt (pos2char,char2pos) xs          
    where
        (ixA, ixB) | rowA == rowB = ((rowA, (colA `mod` 5) + 1), (rowB, (colB `mod` 5) + 1))
                   | colA == colB = (((rowA `mod` 5) + 1, colA), ((rowB `mod` 5) + 1, colB))
                   | otherwise    = ((rowA, colB), (rowB, colA))
        (rowA,colA) = char2pos ! a
        (rowB,colB) = char2pos ! b

digraphs :: String -> [(Char,Char)]
digraphs [] = []
digraphs [x] = [(x,'X')]
digraphs (x:y:xs) | x == y = (x,'X') : digraphs (y:xs)
                  | otherwise = (x,y) : digraphs xs

createTables :: C.ByteString -> (UArray (Int,Int) Char, Array Char (Int,Int))
createTables key = (array ((1,1),(5,5)) order, array ('A','Z') (map swap order))
    where
        set = C.unpack key
            & nub
        order = ['A'..'P'] ++ ['R'..'Z']
            & filter (`notElem` set)
            & (set++)
            & take 25
            & zip (range ((1,1), (5,5)))
