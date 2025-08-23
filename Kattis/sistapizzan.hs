{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow            ((>>>))
import           Data.Bool                (bool)
import qualified Data.ByteString.Char8 as C
import           Data.List                (sort)
import           Data.Maybe               (fromJust)

data OddEven = Odd | Even 
    deriving (Eq, Ord)

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> tail
        >>> map (readInt >>> odd >>> bool Even Odd)
        >>> sort
        >>> bruteForce
        >>> bool "Nej" "Ja"
        >>> C.putStrLn
    )

bruteForce :: [OddEven] -> Bool
bruteForce odds = case odds of
    [Odd]  -> False
    [Even]  -> True
    [Odd,Odd] -> True
    [Odd,Even] -> True
    [Even,Even] -> False
    [Odd,Odd,Odd] -> False
    [Odd,Odd,Even] -> True
    [Odd,Even,Even] -> True
    [Even,Even,Even] -> True
    _ -> error "bruh"

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
