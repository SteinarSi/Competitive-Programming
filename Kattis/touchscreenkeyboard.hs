{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         (on, (&))
import           Data.List             (findIndex, sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> parse
    )

parse :: [C.ByteString] -> IO ()
parse [] = pure ()
parse (x:xs) = C.putStrLn (solve w ys) >> parse zs
    where
        [w, n] = C.words x
        (ys, zs) = splitAt (readInt n) xs

solve :: C.ByteString -> [C.ByteString] -> C.ByteString
solve word = map (\x -> (distance word x, x))
        >>> sort
        >>> map (\(d,x) -> x <> " " <> C.pack (show d))
        >>> C.unlines
        >>> C.init

distance :: C.ByteString -> C.ByteString -> Int
distance word attempt = C.zipWith (manhattan `on` pos) word attempt & sum

pos :: Char -> (Int,Int)
pos c = (x, y)
    where
        y = findIndex (C.elem c) keyboard & fromJust
        x = C.findIndex (c==) (keyboard !! y) & fromJust

keyboard :: [C.ByteString]
keyboard = [
        "qwertyuiop",
        "asdfghjkl",
        "zxcvbnm"
    ]

manhattan :: (Int,Int) -> (Int,Int) -> Int
manhattan (a,b) (x,y) = abs (a-x) + abs (b-y)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
