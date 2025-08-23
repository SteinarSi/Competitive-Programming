{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)
import           Data.Sequence         (Seq (..))
import qualified Data.Sequence         as Seq

main :: IO ()
main = C.interact (
            C.lines
        >>> drop 1
        >>> parse
        >>> map (uncurry (solve False) >>> format)
        >>> C.unlines
    )

solve :: Bool -> String -> Seq C.ByteString -> Maybe [C.ByteString]
solve True  "" xs               = Just (foldl (flip (:)) [] xs)
solve False "" xs               = Just (foldr (:) [] xs)
solve rev ('R':is) xs           = solve (not rev) is xs
solve _ ('D':is) Empty          = Nothing
solve False ('D':is) (x :<| xs) = solve False is xs
solve True  ('D':is) (xs :|> x) = solve True is xs

format :: Maybe [C.ByteString] -> C.ByteString
format Nothing   = "error"
format (Just xs) = "[" <> C.intercalate "," xs <> "]"

parse :: [C.ByteString] -> [(String, Seq C.ByteString)]
parse [] = []
parse (is:_:xs:xss) = (C.unpack is, Seq.fromList (C.split ',' (C.tail (C.init xs)))) : parse xss

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
