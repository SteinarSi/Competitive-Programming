{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (digitToInt)
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (transpose)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> mapM_ (parseOct
            >>> parseState
            >>> eval
            >>> C.putStrLn
        )
    )

data Set = X | O | N
    deriving (Eq, Show)

parseOct :: C.ByteString -> Int
parseOct = C.unpack
    >>> map digitToInt
    >>> reverse
    >>> zipWith (*) (map (8^) [0..])
    >>> sum

parseState :: Int -> [Set]
parseState x = zipWith (\p w -> if
                            | not p     -> N
                            | w         -> X
                            | otherwise -> O
                        ) played who
    where
        (played, who) = splitAt 9 (bin x ++ repeat False)
        bin 0 = []
        bin x = odd x : bin (x `div` 2)

eval :: [Set] -> C.ByteString
eval p | win X      = "X wins"
       | win O      = "O wins"
       | N `elem` p = "In progress"
       | otherwise  = "Cat's"
    where
        win s = [s,s,s] `elem` (rows ++ cols ++ diag)
        rows  = chunksOf 3 p & take 3
        cols  = transpose rows
        diag  = [map (\i->rows!!i!!i) [0..2], map (\i->rows!!(2-i)!!i) [0..2]]

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = a : chunksOf k b
    where (a,b) = splitAt k xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
