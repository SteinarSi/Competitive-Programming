{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = C.getLine >>= C.putStrLn . balance 0 0 0

balance :: Int -> Int -> Int -> B.ByteString -> B.ByteString
balance !w !b !i xs | i >= C.length xs && w == b = "1"
                    | i >= C.length xs && w /= b = "0"
                    | B.index xs i == 87 = balance (w+1) b (i+1) xs
                    | otherwise = balance w (b+1) (i+1) xs
