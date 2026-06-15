{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> reverse
        >>> undo
        >>> backspace
        >>> (">":)
        >>> reverse
        >>> C.concat
        >>> C.putStrLn
    )

undo :: [C.ByteString] -> [C.ByteString]
undo [] = []
undo ["Undo"] = []
undo ("Undo":"Undo":xs) = case undo ("Undo":xs) of
    []     -> []
    (_:ys) -> ys
undo ("Undo":_:xs) = undo xs
undo (x:xs) = x : undo xs

backspace :: [C.ByteString] -> [C.ByteString]
backspace []               = []
backspace ("Backspace":xs) = drop 1 $ backspace xs
backspace (x:xs)           = x : backspace xs
