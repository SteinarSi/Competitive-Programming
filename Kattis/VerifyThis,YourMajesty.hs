{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n <- C.getLine <&> readInt
    queens <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt >>> (\(a:b:_) -> (a,b))))
    C.putStrLn $ if nqueens queens
        then "INCORRECT"
        else "CORRECT"

nqueens :: [(Int,Int)] -> Bool
nqueens []     = False
nqueens (x:xs) = any (attack x) xs || nqueens xs

attack :: (Int, Int) -> (Int, Int) -> Bool
attack (x,y) (a,b) = (x == a) || (y == b) || (abs (x-a) == abs (y-b))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
