{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         (first, second, (***), (>>>))
import           Data.Bits             (shiftR)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.List             (sort)
import           Data.Maybe            (fromJust)
import           Data.Tuple            (swap)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> parse
        >>> map (uncurry solve)
        >>> zipWith (\i s -> "Case " <> C.pack (show i) <> "\n" <> s) [1..]
        >>> C.concat
        >>> C.putStr
    )

solve :: (Int,Int) -> [((Int,Int),C.ByteString)] -> C.ByteString
solve (n,m) = map (first cost)
        >>> sort
        >>> map (\(c,n) -> n <> " " <> C.pack (show c))
        >>> C.unlines
    where
        cost :: (Int,Int) -> Int
        cost (a,b) = takeWhile (shiftR n >>> (>=m)) [0..]
                    & map (\bs -> b*bs + (shiftR n bs - m) * a)
                    & minimum

parse :: [C.ByteString] -> [((Int,Int),[((Int,Int),C.ByteString)])]
parse [] = []
parse (x:xs) = splitAt l xs
        & (map parseOffer >>> ((n,m),)) *** parse
        & uncurry (:)
    where
        [n,m,l] = map readInt (C.words x)

        parseOffer :: C.ByteString -> ((Int,Int),C.ByteString)
        parseOffer = C.span (/=':')
                >>> second (C.drop 1
                    >>> C.readInt
                    >>> fromJust
                    >>> second (C.drop 1 >>> readInt))
                >>> swap

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
