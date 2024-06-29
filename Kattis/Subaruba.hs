{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as C
import           Control.Arrow            ((>>>))
import           Data.Functor             ((<&>))
import           Data.Bool                (bool)
import           Data.Function            ((&))
import           Data.Char                (isUpper, isLower, toUpper)

main :: IO ()
main = do
    mode:_:xs <- C.getContents <&> C.lines

    let f = bool decrypt encrypt (mode == "D")

    xs
        & map (C.words 
            >>> map (C.unpack 
                >>> f 
                >>> C.pack) 
            >>> C.unwords)
        & C.unlines
        & C.putStr

isWovel :: Char -> Bool
isWovel = (`C.elem` "aeiouyAEIOUY")

encrypt :: String -> String
encrypt "" = ""
encrypt (x:xs) | isWovel x && isUpper x = "Ub" ++ (x : encrypt xs)
               | isWovel x && isLower x = "ub" ++ (x : encrypt xs)
               | otherwise = x : encrypt xs

decrypt :: String -> String
decrypt "" = ""
decrypt ('U':'b':x:xs) | isWovel x = toUpper x : decrypt xs
                       | otherwise = "Ub" ++ decrypt (x:xs)
decrypt ('u':'b':x:xs) | isWovel x = x : decrypt xs
                       | otherwise = "ub" ++ decrypt (x:xs)
decrypt (x:xs) = x : decrypt xs
