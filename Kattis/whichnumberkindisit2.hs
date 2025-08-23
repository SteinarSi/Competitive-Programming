{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> map (readInt >>> solve)
        >>> C.unlines
        >>> C.putStr
    )

solve :: Int -> C.ByteString
solve x = case (odd x, square x) of
        (True , False) -> "O"
        (False, True)  -> "S"
        (True , True)  -> "OS"
        (False, False) -> "EMPTY"

square :: Int -> Bool
square x = fromIntegral x
        & sqrt
        & round
        & (^2)
        & (==x)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
