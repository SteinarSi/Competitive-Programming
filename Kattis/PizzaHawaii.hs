{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((***), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.List             (sort)
import           Data.Maybe            (fromJust)
import qualified Data.Set              as S

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> parseMenus
        >>> map solve
        >>> C.intercalate "\n"
        >>> C.putStr
    )

solve :: [(S.Set C.ByteString, S.Set C.ByteString)] -> C.ByteString
solve pizzas = S.unions (map fst pizzas)
        & S.toList
        & map translate
        & sort
        & map format
        & C.concat
    where
        translate :: C.ByteString -> (C.ByteString, S.Set C.ByteString)
        translate for = S.filter (snd >>> (==mask)) locals
                            & S.map fst
                            & (,) for
            where mask = map (fst >>> S.member for) pizzas

        locals :: S.Set (C.ByteString, [Bool])
        locals = map snd pizzas
            & S.unions
            & S.map (\l -> (l, map (snd >>> S.member l) pizzas))

        format :: (C.ByteString, S.Set C.ByteString) -> C.ByteString
        format (for, locs) = S.toAscList locs
            & map (\loc -> C.concat ["(", for, ", ", loc, ")\n"])
            & C.concat

parseMenus :: [C.ByteString] -> [[(S.Set C.ByteString, S.Set C.ByteString)]]
parseMenus [] = []
parseMenus (n:xs) = splitAt (3 * readInt n) xs
    & ((map (C.words >>> tail >>> S.fromList) >>> parsePizzas) *** parseMenus)
    & uncurry (:)

    where
        parsePizzas :: [S.Set C.ByteString] -> [(S.Set C.ByteString, S.Set C.ByteString)]
        parsePizzas []            = []
        parsePizzas (_:xs:ys:xss) = (xs, ys) : parsePizzas xss

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
