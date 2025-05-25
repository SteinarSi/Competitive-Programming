{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((&&&), (***), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.IntMap.Strict    as M
import           Data.Maybe            (fromJust)

type Offer = (C.ByteString,Int,Int)
type Market = (Maybe Int, M.IntMap Int, M.IntMap Int)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map C.words
        >>> parse
        >>> concatMap (solulu (Nothing, M.empty, M.empty))
        >>> C.unlines
        >>> C.putStr
    )

solulu :: Market -> [Offer] -> [C.ByteString]
solulu market []                = []
solulu (price,asks,bids) (x:xs) = (case x of
        ("buy" ,m,p) -> (price, asks, M.insertWith (+) p m bids)
        ("sell",n,p) -> (price, M.insertWith (+) p n asks, bids))
    & simulate
    & format &&& (`solulu` xs)
    & uncurry (:)

format :: Market -> C.ByteString
format (price,asks,bids) = [M.lookupMin asks <&> fst, M.lookupMax bids <&> fst, price]
    & map fm
    & unwords
    & C.pack
  where
    fm :: Show s => Maybe s -> String
    fm Nothing  = "-"
    fm (Just x) = show x

simulate :: Market -> Market
simulate r@(price,asks,bids) = case (M.lookupMin asks, M.lookupMax bids) of
    (Just (a,n), Just (b,m)) | a >  b -> r
                             | n >  m -> simulate (Just a, M.insert a (n-m) asks, M.delete b bids      )
                             | n == m -> simulate (Just a, M.delete a asks      , M.delete b bids      )
                             | n <  m -> simulate (Just a, M.delete a asks      , M.insert b (m-n) bids)
    _ -> r

parse :: [[C.ByteString]] -> [[Offer]]
parse [] = []
parse ((n:_):xs) = splitAt (readInt n) xs
    & map (\[bs,s,_,_,p] -> (bs,readInt s,readInt p)) *** parse
    & uncurry (:)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
