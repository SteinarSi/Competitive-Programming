{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.List             (partition)
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.interact (
            C.lines
        >>> drop 1
        >>> map C.words
        >>> foldr (\[u,v,d] -> M.insertWith (+) u (readInt d) >>> M.insertWith (+) v (- readInt d)) M.empty
        >>> M.assocs
        >>> filter (snd >>> (/=0))
        >>> partition (snd >>> (>0))
        >>> uncurry equalize
        >>> C.unlines
    )

equalize :: [(C.ByteString,Int)] -> [(C.ByteString,Int)] -> [C.ByteString]
equalize [] [] = ["settled"]
equalize [] [x] = error (show x)
equalize [x] [] = error (show x)
equalize ((u,x):xs) ((v,y):ys) = case compare x (-y) of
    LT -> C.unwords [v,u,show' x] : equalize xs ((v,x+y):ys)
    EQ -> C.unwords [v,u,show' x] : equalize xs ys
    GT -> C.unwords [v,u,show' (-y)] : equalize ((u,x+y):xs) ys

show' :: Show s => s -> C.ByteString
show' = show >>> C.pack

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
