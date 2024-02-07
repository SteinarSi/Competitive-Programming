{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as C
import           Data.Char             (toUpper)

main :: IO ()
main = C.getContents >>= putStrLn . keys False False "" . tail . C.lines

keys :: Bool -> Bool -> String -> [C.ByteString] -> String
keys _ _ ret [] = reverse ret
keys s c ret (x:xs) = case x of
    "clank"  -> keys s c (swap 'a':ret) xs
    "bong"   -> keys s c (swap 'b':ret) xs
    "click"  -> keys s c (swap 'c':ret) xs
    "tap"    -> keys s c (swap 'd':ret) xs
    "poing"  -> keys s c (swap 'e':ret) xs
    "clonk"  -> keys s c (swap 'f':ret) xs
    "clack"  -> keys s c (swap 'g':ret) xs
    "ping"   -> keys s c (swap 'h':ret) xs
    "tip"    -> keys s c (swap 'i':ret) xs
    "cloing" -> keys s c (swap 'j':ret) xs
    "tic"    -> keys s c (swap 'k':ret) xs
    "cling"  -> keys s c (swap 'l':ret) xs
    "bing"   -> keys s c (swap 'm':ret) xs
    "pong"   -> keys s c (swap 'n':ret) xs
    "clang"  -> keys s c (swap 'o':ret) xs
    "pang"   -> keys s c (swap 'p':ret) xs
    "clong"  -> keys s c (swap 'q':ret) xs
    "tac"    -> keys s c (swap 'r':ret) xs
    "boing"  -> keys s c (swap 's':ret) xs
    "boink"  -> keys s c (swap 't':ret) xs
    "cloink" -> keys s c (swap 'u':ret) xs
    "rattle" -> keys s c (swap 'v':ret) xs
    "clock"  -> keys s c (swap 'w':ret) xs
    "toc"    -> keys s c (swap 'x':ret) xs
    "clink"  -> keys s c (swap 'y':ret) xs
    "tuc"    -> keys s c (swap 'z':ret) xs
    "whack"  -> keys s c (' ':ret) xs
    "bump"   -> keys s (not c) ret xs
    "pop"    -> keys s c (drop 1 ret) xs
    "dink"   -> keys True c ret xs
    "thumb"  -> keys False c ret xs
    where
        swap :: Char -> Char
        swap k | c /= s    = toUpper k
               | otherwise = k
