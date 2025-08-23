{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (digitToInt)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> init
        >>> map solve
        >>> C.unlines
        >>> C.putStr
    )

solve :: C.ByteString -> C.ByteString
solve xss = case add 0 x y of
        0 -> "No carry operation."
        1 -> "1 carry operation."
        p -> C.pack (show p) <> " carry operations."
    where
        [x,y] = map parse (C.words xss)

parse :: C.ByteString -> [Int]
parse = C.dropWhile (=='0') >>> C.foldl (\rs -> digitToInt >>> (:rs)) []

add :: Int -> [Int] -> [Int] -> Int
add _ [] [] = 0
add c [] (y:ys) | y+c >= 10 = 1 + add 1 [] ys
                | otherwise = 0
add c (x:xs) [] | x+c >= 10 = 1 + add 1 xs []
                | otherwise = 0
add c (x:xs) (y:ys) | x+y+c >= 10 = 1 + add 1 xs ys
                    | otherwise   =     add 0 xs ys
