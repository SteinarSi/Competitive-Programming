{-# LANGUAGE BangPatterns #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.List             (sortOn)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> tail
        >>> map readInt
        >>> sortOn negate
        >>> solve (0,0)
        >>> C.putStrLn
    )

solve :: (Int,Int) -> [Int] -> C.ByteString
solve (!a,!b) []       = C.unwords $ map (show >>> C.pack) [a,b]
solve (!a,!b) [x]      = solve (a+x,b) []
solve (!a,!b) (x:y:xs) = solve (a+x,b+y) xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
