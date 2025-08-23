{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n <- C.getContents <&> readInt

    let (q,r) = quotRem n 3
        (n',out) = case r of
            0 -> (q, threes q)
            1 -> (q+1, threes (q-1) <> " 2 2")
            2 -> (q+1, threes q <> " 2")

    C.putStrLn (C.pack (show n') <> "\n" <> out)

-- Somehow this is significantly faster than just
--  C.concat (replicate x "3 ")
threes :: Int -> C.ByteString
threes x = C.intersperse ' ' (C.replicate x '3')

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
