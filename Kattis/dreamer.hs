{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (digitToInt, isDigit)
import           Data.Function         ((&))
import           Data.List             (permutations)
import           Data.Maybe            (fromJust, listToMaybe)
import qualified Data.Set              as S

type Date = (Int,Int,Int)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> mapM_ (C.filter isDigit
            >>> C.unpack
            >>> map digitToInt
            >>> solve
            >>> C.putStrLn
        )
    )

solve :: [Int] -> C.ByteString
solve xs | S.null ps   = "0"
         | otherwise = [show (length ps), format (fromJust (S.lookupMin ps))]
             & map C.pack
             & C.unwords
    where
        ps :: S.Set Date
        ps = permutations xs
            & map date
            & filter valid
            & S.fromList

date :: [Int] -> Date
date [a,b,c,d,e,f,g,h] = (year, month, day)
    where day   = a*10 + b
          month = c*10 + d
          year  = e*1000 + f*100 + g*10 + h

format :: Date -> String
format (year, month, day) = [day, month, year] & map f & unwords
    where f x | x < 10 = '0' : show x
              | otherwise = show x

valid :: (Int, Int, Int) -> Bool
valid (year, month, day) = and [
        year >= 2000,
        month >= 1,
        month <= 12,
        day >= 1,
        day <= days
    ]
    where
          leap  = year `mod` 400 == 0 || year `mod` 100 /= 0 && year `mod` 4 == 0
          days  | month `elem` [1,3,5,7,8,10,12] = 31
                | month == 2 && leap = 29
                | month == 2 = 28
                | otherwise = 30
