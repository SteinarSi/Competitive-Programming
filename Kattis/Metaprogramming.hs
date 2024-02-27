{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (toLower)
import           Data.Map              (Map, empty, insert, lookup)
import           Prelude               hiding (lookup)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> map C.words
        >>> solve empty
        >>> mapM_ C.putStrLn
    )

solve :: Map C.ByteString Int -> [[C.ByteString]] -> [C.ByteString]
solve defs []                     = []
solve defs (["define", i, x]:xs)  = solve (insert x (read (C.unpack i)) defs) xs
solve defs (["eval", x, y, z]:xs) = case (lookup x defs, lookup z defs) of
    (Just a, Just b) -> C.pack (map toLower (show (a `op` b))) : solve defs xs
    _                -> "undefined" : solve defs xs
    where op | y == "<"  = (<)
             | y == "="  = (==)
             | otherwise = (>)
