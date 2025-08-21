{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((***), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import qualified Data.IntMap.Lazy      as M
import           Data.Maybe            (fromJust)

data Chapter = End Bool | Choice [Int]

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map C.words
        >>> parse
        >>> map (solve >>> show)
        >>> unlines
        >>> putStr
    )

solve :: M.IntMap Chapter -> Int
solve dag = dp M.! 1
  where
    dp :: M.IntMap Int
    dp = M.map (\case
            End True  -> 1
            End False -> 0
            Choice vs -> sum (map (dp M.!) vs)
        ) dag

parse :: [[C.ByteString]] -> [M.IntMap Chapter]
parse [] = []
parse ([n]:xs) = splitAt (readInt n) xs
    & (map pars >>> M.fromList)
        ***
        parse
    & uncurry (:)
  where
    pars :: [C.ByteString] -> (Int, Chapter)
    pars [u,fate] = (readInt u, End (fate == "favourably"))
    pars (u:vs)   = (readInt u, Choice (map readInt vs))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
