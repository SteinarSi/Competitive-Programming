{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Control.Monad         (forM_)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.Map              as M
import           Data.Maybe            (fromJust)
import qualified Data.Set              as S

main :: IO ()
main = do
    n <- C.getLine <&> readInt
    names <- C.getContents <&> C.lines
    let home = take n names
        one = filter (C.notElem ' ') home
            & S.fromList
        two = filter (C.elem ' ') home
            & map (\x -> (C.takeWhile (/=' ') x, x))
            & M.fromList
        queries = drop (n+1) names

    forM_ queries $ \q -> C.putStrLn $ case (S.member q one, M.lookup (C.takeWhile (/=' ') q) two) of
        (True, _) -> "Jebb"
        (False, Nothing) -> "Neibb"
        (False, Just x) | q == x -> "Jebb"
                        | otherwise -> "Neibb en " <> x <> " er heima"

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
