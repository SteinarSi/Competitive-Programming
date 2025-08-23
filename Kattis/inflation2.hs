{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.IntMap           as M
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:ps:_:qs <- C.getContents <&> C.lines

    let xs = map readInt (C.words ps)
        count = M.fromListWith (+) (map (,1) xs)

    inflation (readInt n) 0 (sum xs) count (map C.words qs)
        & tail
        & map (show >>> C.pack)
        & C.unlines
        & C.putStr

inflation :: Int -> Int -> Int -> M.IntMap Int -> [[C.ByteString]] -> [Int]
inflation n infl cost count [] = [n*infl + cost]
inflation n infl cost count (["INFLATION", x]:qs) = (n*infl + cost) : inflation n (infl+readInt x) cost count qs
inflation n infl cost count (["SET",x,y]:qs) = (n*infl + cost) : case M.lookup (x'-infl) count of
        Nothing -> inflation n infl cost count qs
        Just m  -> inflation n infl (cost - m*x' + m*y') (M.insertWith (+) (y'-infl) m (M.delete (x'-infl) count)) qs
    where (x',y') = (readInt x, readInt y)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
