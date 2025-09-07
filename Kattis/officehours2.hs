{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((***), (>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (STArray, getAssocs, newArray, readArray,
                                        writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import qualified Data.IntSet           as S
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.interact (
            C.lines
        >>> drop 1
        >>> map C.words
        >>> parseTimes M.empty
        >>> count
        >>> office ((undefined,undefined),0)
    )

office :: ((Int,Int),Int) -> [(Int,S.IntSet)] -> C.ByteString
office ((bi,bj),_) [] = C.unlines (map formatTime [bi,bj])
office bij@(_,b) ((i,x):xs)
    | S.null x || nv <= b = office bij xs
    | otherwise = office ((i,k),nv) xs
  where
    sx = S.size x
    (k,nv) = hours (undefined,b) xs

    hours :: (Int,Int) -> [(Int,S.IntSet)] -> (Int,Int)
    hours (bk,bv) [] = (bk,bv)
    hours (bk,bv) ((j,y):ys)
        | S.null y || sx + S.size y <= bv || v <= bv = hours (bk,bv) ys
        | otherwise = hours (j,v) ys
      where
        v = S.size (S.union x y)

count :: [(Int,Int)] -> [(Int,S.IntSet)]
count xs = runST $ do
    ret <- newArray (0,6*24+23) S.empty :: ST s (STArray s Int S.IntSet)
    mapM_ (\(t,n) -> readArray ret t >>= (S.insert n >>> writeArray ret t)) xs
    getAssocs ret

parseTimes :: M.Map C.ByteString Int -> [[C.ByteString]] -> [(Int,Int)]
parseTimes _ []                = []
parseTimes ns ((n:d:_:xs):xss) = concatMap parseTime xs <> parseTimes ns' xss
  where
    di = 24 * day2int M.! d
    (id,ns') = case M.lookup n ns of
        Nothing -> (M.size ns, M.insert n (M.size ns) ns)
        Just  i -> (i, ns)

    parseTime :: C.ByteString -> [(Int,Int)]
    parseTime x = let [a,b] = map readInt (C.split '-' x)
                  in  map (\t -> (di+t, id)) [a..b-1]

formatTime :: Int -> C.ByteString
formatTime = (`quotRem` 24) >>> (weekdays!!) *** (show >>> (' ':) >>> C.pack) >>> uncurry (<>)

day2int :: M.Map C.ByteString Int
day2int = M.fromList (zip weekdays [0..])

weekdays :: [C.ByteString]
weekdays = ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"]

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
