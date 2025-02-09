{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Data.Array            (Array, listArray, (!))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Char             (toLower)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (vow >>> bool "NO" "YES")
        >>> unlines
        >>> putStr
    )

vow :: C.ByteString -> Bool
vow text = dp ! 0
    where
        n = C.length text

        dp :: Array Int Bool
        dp = listArray (0, n) (map f [0..n])

        f :: Int -> Bool
        f i | i == n    = True
            | otherwise = any viable symbols
            where
                viable :: C.ByteString -> Bool
                viable symbol = let len1 = C.length symbol == 1 && text `C.index` i == C.head symbol
                                    len2 = i < n-1 && text `C.index` i == C.head symbol && text `C.index` (i+1) == symbol `C.index` 1
                                    next = dp ! (i + C.length symbol)
                                in  (len1 || len2) && next

symbols :: [C.ByteString]
symbols = [
    "h","he",
    "li","be","b","c","n","o","f","ne",
    "na","mg","al","si","p","s","cl","ar",
    "k","ca","sc","ti","v","cr","mn","fe","co","ni","cu","zn","ga","ge","as","se","br","kr",
    "rb","sr","y","zr","nb","mo","tc","ru","rh","pd","ag","cd","in","sn","sb","te","i","xe",
    "cs","ba","hf","ta","w","re","os","ir","pt","au","hg","tl","pb","bi","po","at","rn",
    "fr","ra","rf","db","sg","bh","hs","mt","ds","rg","cn","fl","lv",
    "la","ce","pr","nd","pm","sm","eu","gd","tb","dy","ho","er","tm","yb","lu",
    "ac","th","pa","u","np","pu","am","cm","bk","cf","es","fm","md","no","lr"
    ]
