{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Control.Monad         (ap, when)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)
import           System.Exit           (exitSuccess)
import           System.IO             (hFlush, stdout)

main :: IO ()
main = do
    n <- C.getLine <&> readInt
    guess 2 3
    guess 2 4
    if n <= 4
        then do
            guess 1 2
            guess 1 3
            guess 1 4
            guess 3 4
        else do
            guess 1 5
            guess 1 6
            mapM_ (ap guess pred) [n,n-2 .. 1]

guess :: Int -> Int -> IO ()
guess i j = do
    putStrLn (show i <> " " <> show j)
    hFlush stdout
    response <- C.getLine
    when (response == "Ljos!") exitSuccess

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
