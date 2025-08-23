{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as C
import           Control.Arrow            ((>>>))
import           Data.Char                (digitToInt, isDigit, ord, chr)
import           Data.Function            ((&))
import           Data.Bool                (bool)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> mapM_ (C.words >>> solve >>> C.putStrLn)
    )

solve :: [C.ByteString] -> C.ByteString
solve [x,o,y,_,z] = [2..36]
        & filter (\b -> all (<b) ds && all (interpret b >>> (<2^31)) xyz)
        & bool id (1:) (all (==1) ds)
        & filter valid
        & format
    where 
        xyz@[x',y',z']  = map digits [x,y,z]
        ds = concat xyz
        valid b = let [x'',y'',z''] = map (interpret b) xyz
                  in  case o of
                        "+" -> x'' + y'' == z''
                        "-" -> x'' - y'' == z''
                        "*" -> x'' * y'' == z''
                        "/" -> x'' `div` y'' == z'' && z'' * y'' == x''

format :: [Int] -> C.ByteString
format [] = "invalid"
format xs = map f xs 
        & C.pack
    where
        f 36            = '0'
        f x | x < 10    = head (show x)
            | otherwise = chr (x + ord 'a' - 10)

interpret :: Int -> [Int] -> Int
interpret 1 = length
interpret b = zipWith (*) (map (b^) [0..]) >>> sum

digits :: C.ByteString -> [Int]
digits = C.foldr (digit >>> (:)) [] >>> reverse
    where 
        digit :: Char -> Int
        digit x | isDigit x = digitToInt x
                | otherwise = ord x - ord 'a' + 10