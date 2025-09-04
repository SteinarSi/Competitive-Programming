{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (STUArray, newArray, readArray,
                                        writeArray)
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Char             (isAlpha)

main :: IO ()
main = C.interact (
            C.lines
        >>> filter (C.head >>> isAlpha)
        >>> map solve
        >>> C.unlines
    )

solve :: C.ByteString -> C.ByteString
solve xs = case runST (newArray ('a','z') (-1) >>= \prev -> search prev (Nothing,maxBound) 0) of
    Nothing    -> "-1"
    Just (j,i) -> C.take j xs <> C.drop i xs
  where
    n = C.length xs

    search :: STUArray s Char Int -> (Maybe (Int,Int), Int) -> Int -> ST s (Maybe (Int,Int))
    search prev (d,b) i
        | i >= n = pure d
        | otherwise = do
            j <- readArray prev (C.index xs i)
            writeArray prev (C.index xs i) i
            search prev (bool (d,b) (Just (j,i),i-j) (j >= 0 && i-j < b)) (i+1)
