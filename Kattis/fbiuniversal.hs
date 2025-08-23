{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> mapM_ (C.words
            >>> (\(a:b:_) -> a <> " " <> solve b)
            >>> C.putStrLn
        )
    )

solve :: C.ByteString -> C.ByteString
solve xs | che == last ds = init ds & decimal & show & C.pack
         | otherwise = "Invalid"
    where
        ds = C.map confuse xs
            & C.unpack
            & map ((`C.elemIndex` "0123456789ACDEFHJKLMNPRTVWX") >>> fromJust)
        che = init ds
            & zipWith (*) [2,4,5,7,8,10,11,13]
            & sum
            & (`mod` 27)

decimal :: [Int] -> Int
decimal = reverse >>> zipWith (*) (map (27^) [0..]) >>> sum

confuse :: Char -> Char
confuse 'B' = '8'
confuse 'G' = 'C'
confuse 'I' = '1'
confuse 'O' = '0'
confuse 'Q' = '0'
confuse 'S' = '5'
confuse 'U' = 'V'
confuse 'Y' = 'V'
confuse 'Z' = '2'
confuse  x  =  x
