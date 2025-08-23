{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Control.Monad         (forM_)
import           Control.Monad.ST      (ST)
import           Data.Array.Base       (STUArray, UArray, elems, newArray,
                                        readArray, writeArray)
import           Data.Array.ST         (runSTUArray)
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> mapM_ (solve >>> format >>> C.putStrLn)
    )

solve :: C.ByteString -> UArray Int Int
solve xs = runSTUArray $ do
    press <- newArray (1,10) 0
    alto [] press (C.unpack xs)
    pure press
    where
        alto :: [Int] -> STUArray s Int Int -> String -> ST s ()
        alto prev press "" = pure ()
        alto prev press (x:xs) = do
            forM_ (filter (`notElem` prev) (buttons x)) $ \i -> readArray press i >>= writeArray press i . succ
            alto (buttons x) press xs

format :: UArray Int Int -> C.ByteString
format = elems >>> map (show >>> C.pack) >>> C.unwords

buttons :: Char -> [Int]
buttons c = case c of
    'c' -> [2..4] ++ [7..10]
    'd' -> [2..4] ++ [7..9]
    'e' -> [2..4] ++ [7,8]
    'f' -> [2..4] ++ [7]
    'g' -> [2..4]
    'a' -> [2,3]
    'b' -> [2]
    'C' -> [3]
    'D' -> [1..4] ++ [7..9]
    'E' -> [1..4] ++ [7, 8]
    'F' -> [1..4] ++ [7]
    'G' -> [1..4]
    'A' -> [1..3]
    'B' -> [1,2]
