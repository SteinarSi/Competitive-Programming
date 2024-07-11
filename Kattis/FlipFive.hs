{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow            (second, (>>>))
import           Data.Bits                (xor)
import           Data.Bool                (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function            ((&))
import qualified Data.IntSet           as S

main :: IO ()
main = C.getContents >>= (
            C.filter (`C.elem` ".*")
        >>> chunksOf 9
        >>> mapM_ (parse 
            >>> S.singleton
            >>> bfs S.empty 
            >>> print)
    )

parse :: C.ByteString -> Int
parse = C.unpack 
    >>> map (('*'==) >>> bool 0 1)
    >>> zipWith (*) (map (2^) [0..])
    >>> sum

bfs :: S.IntSet -> S.IntSet -> Int
bfs seen curr | 0 `S.member` curr = 0
              | otherwise = 1 + bfs (S.union seen next) next
    where
        next = S.foldr (xor 
                >>> map 
                >>> ($ moves) 
                >>> filter (`S.notMember` seen) 
                >>> S.fromList 
                >>> S.union
            ) S.empty curr

moves :: [Int]
moves = [11, 23, 38, 89, 186, 308, 200, 464, 416]

chunksOf :: Int -> C.ByteString -> [C.ByteString]
chunksOf k xs | C.null xs = []
              | otherwise = C.splitAt k xs
                    & second (chunksOf k)
                    & uncurry (:)
