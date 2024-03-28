{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Char             (ord)

main :: IO ()
main = C.getContents >>= (C.lines
        >>> mapM_ (C.unpack
            >>> map (ord >>> press 0)
            >>> foldr (\(a,b) (x,y) -> (a+x, b+y)) (0,0)
            >>> (\(a,b) -> even a && even b)
            >>> bool "trapped" "free"
            >>> C.putStrLn
        )
    )

press :: Int -> Int -> (Int,Int)
press a 0 = (a, 7 - a)
press a x | odd x = press (a+1) (x `div` 2)
          | otherwise = press a (x `div` 2)
