{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Char             (isUpper, ord, toLower, toUpper)
import           Data.Function         (on)
import           Data.List             (minimumBy)

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> map nimionese
        >>> C.unwords
        >>> C.putStrLn
    )

nimionese :: C.ByteString -> C.ByteString
nimionese = hardenFirst >>> hardenSubsequent >>> softenLast

hardenFirst :: C.ByteString -> C.ByteString
hardenFirst xs = C.cons (harden (C.head xs)) (C.tail xs)

hardenSubsequent :: C.ByteString -> C.ByteString
hardenSubsequent xs = C.concat (x : map (C.map harden) xss)
    where
        x:xss = C.split '-' xs

        harden :: Char -> Char
        harden c | c `notElem` hard = c
                 | isUpper c = toUpper (C.head x)
                 | otherwise = toLower (C.head x)


softenLast :: C.ByteString -> C.ByteString
softenLast xs | toLower l `elem` hard = xs <> minimumBy (compare `on` (\s -> (abs (ord l - ord (C.head s)), s))) soft
              | otherwise = xs
    where l = C.last xs

hard :: String
hard = "bcdgknpt"

harden :: Char -> Char
harden c | isUpper c = toUpper h
         | otherwise = h
    where h = minimumBy (compare `on` (\d -> (abs (ord (toLower c) - ord d), d))) hard

soft :: [C.ByteString]
soft = ["ah","oh","uh"]
