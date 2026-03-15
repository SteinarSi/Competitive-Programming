{-# LANGUAGE OverloadedStrings #-}

import           Data.Array            (Array, listArray, (!))
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = do
    x <- getLine
    C.putStrLn ("{" <> neumann ! read x <> "}")

neumann :: Array Int C.ByteString
neumann = listArray (0,20) ("" : "{}" : map f [2..20])
  where
    f x = neumann ! (x-1) <> ",{" <> neumann ! (x-1) <> "}"
