{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         (first, (***), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Maybe            (fromJust)
import           Prelude               hiding (gcd)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> map C.words
        >>> parse
        >>> concatMap (first perform >>> uncurry map >>> map (show >>> C.pack))
        >>> C.unlines
        >>> C.putStr
    )

parse :: [[C.ByteString]] -> [(Int, [[C.ByteString]])]
parse [] = []
parse [_] = []
parse ([n,t]:xs) = splitAt (readInt t) xs
        & (,) (readInt n) *** parse
        & uncurry (:)

perform :: Int -> [C.ByteString] -> Int
perform n [a,op,b] = case op of
    "+" -> (a' + b') `mod` n
    "-" -> (a' - b') `mod` n
    "*" -> (a' * b') `mod` n
    "/" | g == 1 -> (a' * s) `mod` n
        | otherwise -> -1

    where
        a' = readInt a
        b' = readInt b
        (s, _, g) = gcd b' n

gcd :: Int -> Int -> (Int, Int, Int)
gcd a 0 = (1, 0, a)
gcd a b = (t, s - q * t, g)
    where
        (q, r) = a `quotRem` b
        (s, t, g) = gcd b r

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
