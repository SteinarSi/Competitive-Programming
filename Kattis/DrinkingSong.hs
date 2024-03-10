{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Control.Monad         (replicateM)
import qualified Data.ByteString.Char8 as C
import           Data.Function         (on, (&))
import           Data.List             (intersperse)

main :: IO ()
main = do
    n <- fmap read getLine
    drink <- C.getLine
    solve n drink
        & intersperse ""
        & C.unlines
        & C.putStr

solve :: Int -> C.ByteString -> [C.ByteString]
solve 1 drink = [C.unwords [
        "1 bottle of",
        drink,
        "on the wall, 1 bottle of",
        drink <> "."
    ]
    <> "\n"
    <> C.unwords [
        "Take it down, pass it around, no more bottles of",
        drink <> "."
        ]
    ]
solve 2 drink = C.unwords [
        "2 bottles of",
        drink,
        "on the wall, 2 bottles of",
        drink <> "."
    ]
    <> "\n"
    <> C.unwords [
        "Take one down, pass it around, 1 bottle of",
        drink,
        "on the wall."
    ] : solve 1 drink
solve n drink = C.unwords [
        C.pack (show n),
        "bottles of",
        drink,
        "on the wall,",
        C.pack (show n),
        "bottles of",
        drink <> "."
    ]
    <> "\n"
    <> C.unwords [
        "Take one down, pass it around,",
        C.pack (show (n-1)),
        "bottles of",
        drink,
        "on the wall."
    ] : solve (n-1) drink
