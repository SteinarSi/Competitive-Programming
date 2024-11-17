{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         (first, second, (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         (on)
import           Data.List             (maximumBy, sortOn)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> flip zip [1..]
        >>> map (first score)
        >>> maximumBy (compare `on` second negate)
        >>> snd
        >>> print
    )

score :: C.ByteString -> Int
score = C.words
    >>> zipWith (==) fizzbuzz
    >>> filter id
    >>> length

fizzbuzz :: [C.ByteString]
fizzbuzz = map fizz [1..]
    where
        fizz x = case (x `mod` 3 == 0, x `mod` 5 == 0) of
            (True , True ) -> "fizzbuzz"
            (True , False) -> "fizz"
            (False, True ) -> "buzz"
            (False, False) -> C.pack (show x)
