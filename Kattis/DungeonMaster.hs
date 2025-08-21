{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         (second, (***), (>>>))
import           Control.Monad         (filterM, unless)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray, UArray, assocs, bounds,
                                        listArray, newArray, readArray,
                                        writeArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Ix               (inRange)
import           Data.List             (find)
import           Data.Maybe            (fromJust)
import           Data.Sequence         (Seq (..))
import qualified Data.Sequence         as Seq
import           Text.Printf           (printf)

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> parse
        >>> map (escape >>> maybe "Trapped!" (printf "Escaped in %d minute(s)."))
        >>> unlines
        >>> putStr
    )

escape :: UArray (Int,Int,Int) Char -> Maybe Int
escape graph = runST $ do
    seen <- newArray rng False
    writeArray seen s True
    search seen (Seq.singleton (0,s))
  where
    rng = bounds graph
    Just (s,_) = find (snd >>> (=='S')) (assocs graph)

    search :: STUArray s (Int,Int,Int) Bool -> Seq (Int,(Int,Int,Int)) -> ST s (Maybe Int)
    search seen Empty = pure Nothing
    search seen ((d,(z,y,x)) :<| xs)
        | graph ! (z,y,x) == 'E' = pure (Just d)
        | otherwise = [(z-1,y,x),(z+1,y,x),(z,y-1,x),(z,y+1,x),(z,y,x-1),(z,y,x+1)]
            & filter (inRange rng)
            & filter ((graph!) >>> (/='#'))
            & filterM (\v -> readArray seen v >>= \s -> unless s (writeArray seen v True) >> pure (not s))
            >>= (map (d+1,) >>> Seq.fromList >>> (xs<>) >>> search seen)

parse :: [C.ByteString] -> [UArray (Int,Int,Int) Char]
parse []         = []
parse ("0":_)    = []
parse (l:r:c:xs) = splitAt (z*y) xs
    & (concatMap C.unpack >>> listArray ((1,1,1),(z,y,x))) *** parse
    & uncurry (:)
  where
    [z,y,x] = map readInt [l,r,c]

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = splitAt k xs
        & second (chunksOf k)
        & uncurry (:)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
