{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> split
        >>> map (uncurry intuitive >>> bool "NO" "YES")
        >>> C.unlines
        >>> C.putStr
    )

intuitive :: C.ByteString -> C.ByteString -> Bool
intuitive name = C.all (`C.elem` name)

split :: [a] -> [(a,a)]
split []          = []
split (xs:ys:xss) = (xs,ys) : split xss
