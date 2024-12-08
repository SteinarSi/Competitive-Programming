{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))

main :: IO ()
main = do
    [n,k] <- getContents <&> (words >>> map read)

    C.putStr $ if k >= n
        then "impossible\n"
        else C.unlines (take k strategy)

strategy :: [C.ByteString]
strategy = map (\x -> "open " <> C.pack (show x) <> " using 1") [2..]
