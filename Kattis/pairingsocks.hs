{-# LANGUAGE BangPatterns #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> tail
        >>> map (C.readInt >>> fromJust >>> fst)
        >>> pair 0 []
        >>> maybe "impossible" show
        >>> C.pack
        >>> C.putStrLn
    )

pair :: Int -> [Int] -> [Int] -> Maybe Int
pair !r []     []                 = Just r
pair _  _      []                 = Nothing
pair !r []     (x:xs)             = pair (succ r) [x] xs
pair !r (y:ys) (x:xs) | x == y    = pair (succ r) ys xs
                      | otherwise = pair (succ r) (x:y:ys) xs
