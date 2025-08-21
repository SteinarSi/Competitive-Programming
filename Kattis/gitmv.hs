{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         (second, (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))

main :: IO ()
main = do
    [xs,ys] <- C.getContents <&> (C.lines >>> map (C.splitWith (=='/')))

    let i = countEquals xs ys
        j = countEquals (reverse xs) (reverse ys)
        (a,b) = splitAt i xs
            & second (take (length xs - i - j))
        (c,d) = splitAt (length ys - i - j) (drop i ys)

    a <> ["{" <> C.intercalate "/" b <> " => " <> C.intercalate "/" c <> "}"] <> d
        & C.intercalate "/"
        & C.putStrLn

countEquals :: [C.ByteString] -> [C.ByteString] -> Int
countEquals = zipWith (==) >>> (>>> takeWhile id >>> length)
