{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Control.Monad         (forM_, replicateM, unless)
import qualified Data.ByteString.Char8 as C
import           Data.Char             (toUpper)
import           Data.Functor          ((<&>))
import           Data.Map              (Map, findWithDefault, fromList)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [m, n] <- fmap (C.words >>> map readInt) C.getLine
    values <- replicateM m C.getLine <&> (map (C.words >>> (\(a:b:_) -> (a, readInt b))) >>> fromList )
    candidates <- fmap (C.lines >>> splitOn "." >>> map (concatMap C.words)) C.getContents
    forM_ candidates (evaluate values >>> print)

evaluate :: Map C.ByteString Int -> [C.ByteString] -> Int
evaluate vals = map (flip (findWithDefault 0) vals) >>> sum

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn p (x:xs) | p == x    = splitOn p xs
                 | otherwise = (x : takeWhile (/=p) xs) : splitOn p (dropWhile (/=p) xs)
