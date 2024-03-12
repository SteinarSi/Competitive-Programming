{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Data.Array.Unboxed    (UArray, listArray, (!))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.IntMap           (assocs, empty, insertWith)
import           Data.IntSet           (fromList, member, size)
import           Data.Ix               (inRange)
import           Data.List             (find, sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n,t] <- C.getLine <&> (C.words >>> map readInt)
    xs <- C.getLine <&> (C.words >>> map readInt)
    let a = listArray (0,n-1) xs :: UArray Int Int
        as = sort xs
    C.putStrLn $ case t of
        1 -> let os = fromList (filter odd xs)
             in  filter even xs
                & any (\x -> member (7777-x) os)
                & bool "No" "Yes"
        2 -> xs
            & fromList
            & size
            & (n==)
            & bool "Contains duplicate" "Unique"
        3 -> xs
            & foldr (\a -> insertWith (+) a 1) empty
            & assocs
            & find (snd >>> (>(n`div`2)))
            & maybe (-1) fst
            & show
            & C.pack
        4 | even n -> C.unwords $ map (show >>> C.pack) [as !! ((n-1)`div`2), as !! (n`div`2)]
          | odd  n -> C.pack (show (as !! (n `div` 2)))
        5 -> as
            & filter (inRange (100,999))
            & map (show >>> C.pack)
            & C.unwords

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
