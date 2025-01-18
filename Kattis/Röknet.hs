{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         (on, (&))
import           Data.Functor          ((<&>))
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> solve M.empty
        >>> C.unlines
        >>> C.putStr
    )

solve :: M.Map C.ByteString Bool -> [C.ByteString] -> [C.ByteString]
solve values [] = []
solve values (x:xs) = case C.words x of
    ["INNTAK", a, v] -> solve (M.insert a (v == "SATT") values) xs
    ["UTTAK",a] -> values M.! a
            & bool "OSATT" "SATT"
            & ((a <> " ") <>)
            & (:solve values xs)
    ["OG",a,b,o] -> solve (M.insert o (on (&&) (values M.!) a b) values) xs
    ["EDA",a,b,o] -> solve (M.insert o (on (||) (values M.!) a b) values) xs
    ["EKKI",a,o] -> solve (M.insert o (not (values M.! a)) values) xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
