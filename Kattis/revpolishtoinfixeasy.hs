{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (isDigit)

main :: IO ()
main = C.interact (C.words >>> solve [])

solve :: [C.ByteString] -> [C.ByteString] -> C.ByteString
solve [r] [] = r
solve rs (x:xs) | isDigit (C.head x) = solve (x:rs) xs
solve (a:b:rs) (x:xs) = solve (C.concat ["(", b, x, a, ")"] : rs) xs
