{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> init
        >>> map (solve >>> show)
        >>> unlines
        >>> putStr
    )

solve :: C.ByteString -> Int
solve "1" = 1
solve  x  = re (C.length x)
  where
    re x0 = let x1 = length (show x0)
            in  bool (1 + re x1) 2 (x0 == x1)
