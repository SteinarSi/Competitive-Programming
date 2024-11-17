{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow            ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function            ((&))
import           Data.Maybe               (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.readInt
        >>> fromJust
        >>> fst
        >>> flip take generated
        >>> C.unlines
        >>> C.putStr
    )

generated :: [C.ByteString]
generated = addVowels [""]
    & takeWhile (C.length >>> (<=20))
    & filter (C.length >>> (>=3))

addVowels :: [C.ByteString] -> [C.ByteString]
addVowels xs = ys ++ addConsonants ys
    where ys = map (<>) (section vowels) <*> xs

addConsonants :: [C.ByteString] -> [C.ByteString]
addConsonants xs = ys ++ addVowels ys
    where ys = map (<>) (section consonants) <*> xs

section :: C.ByteString -> [C.ByteString]
section alphabet = singles ++ (map (<>) singles <*> singles)
    where
        singles = C.foldr (pure >>> C.pack >>> (:)) [] alphabet

consonants :: C.ByteString
consonants = ['a'..'z']
    & filter (`C.notElem` vowels)
    & C.pack

vowels :: C.ByteString
vowels = "aeiou"
