{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Data.Bifunctor        (bimap)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromJust)
import           Data.Tuple            (swap)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> mapM_ (solve >>> C.putStrLn)
    )

solve :: C.ByteString -> C.ByteString
solve xs = decrypt lengths (C.concat codes) & C.pack
    where
        a = decrypt lengths (C.concat codes) & C.pack
        codes = C.foldr ((char2code M.!) >>> (:)) [] xs
        lengths = map C.length codes & reverse

        decrypt :: [Int] -> C.ByteString -> String
        decrypt [] _ = ""
        decrypt (x:xs) code = C.splitAt x code
            & bimap (code2Char M.!) (decrypt xs)
            & uncurry (:)

code2Char :: M.Map C.ByteString Char
code2Char = char2code
    & M.assocs
    & map swap
    & M.fromList

char2code :: M.Map Char C.ByteString
char2code = M.fromList [
    ('A', ".-"),
    ('H',"...."),
    ('O',"---"),
    ('V',"...-"),
    ('B',"-..."),
    ('I',".."),
    ('P',".--."),
    ('W',".--"),
    ('C',"-.-."),
    ('J',".---"),
    ('Q',"--.-"),
    ('X',"-..-"),
    ('D',"-.."),
    ('K',"-.-"),
    ('R',".-."),
    ('Y',"-.--"),
    ('E',"."),
    ('L',".-.."),
    ('S',"..."),
    ('Z',"--.."),
    ('F',"..-."),
    ('M',"--"),
    ('T',"-"),
    ('G',"--."),
    ('N',"-."),
    ('U',"..-"),
    ('_',"..--"),
    ('.',"---."),
    (',',".-.-"),
    ('?',"----")
    ]
