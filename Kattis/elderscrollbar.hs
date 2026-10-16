{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    whfn:rest <- C.getContents <&> (C.lines >>> map C.words)
    let [w,h,f,n] = map readInt whfn
        rows = distribute w [] 0 (concat rest)
        l = length rows
        viewport = rows
            & map (\x -> "|" <> x <> C.replicate (w - C.length x) ' ' <> "|")
            & drop f
            & take h
        t = (h-3) * f `div` (l-h)
        top = "+" <> C.replicate w '-' <> "+-+"
        right = "^|" : replicate t " |" <> ["X|"] <> replicate (h-t-3) " |" <> ["v|"]

    (top : zipWith (<>) viewport right <> [top])
        & C.unlines
        & C.putStr

distribute :: Int -> [C.ByteString] -> Int -> [C.ByteString] -> [C.ByteString]
distribute _ [] _ []   = []
distribute w curr c [] = [C.unwords (reverse curr)]
distribute w [] _ (x:xs) = case compare (C.length x) w of
    LT -> distribute w [x] (C.length x) xs
    EQ -> x : distribute w [] 0 xs
    GT -> C.take w x : distribute w [] 0 xs
distribute w curr c (x:xs)
    | c + 1 + C.length x <= w = distribute w (x:curr) (c + 1 + C.length x) xs
    | otherwise = C.unwords (reverse curr) : distribute w [] 0 (x:xs)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
