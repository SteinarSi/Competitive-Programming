{-# LANGUAGE TupleSections #-}

import           Control.Arrow         (second, (>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (STUArray, inRange, newArray, readArray,
                                        writeArray)
import           Data.Array.Unboxed    (UArray, assocs, bounds, elems,
                                        listArray, (//))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (isControl)
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (catMaybes, fromJust)

main :: IO ()
main = do
    [n,m] <- C.getLine <&> (C.words >>> map readInt)
    graph <- C.getContents <&> (C.filter (isControl >>> not) >>> C.unpack >>> listArray ((1,1),(n,m)))

    findSpots graph
        & map (,'E')
        & (graph //)
        & elems
        & chunksOf m
        & map C.pack
        & C.unlines
        & C.putStr

findSpots :: UArray (Int,Int) Char -> [(Int,Int)]
findSpots graph = runST $ do
    covered <- newArray (bounds graph) False :: ST s (STUArray s (Int,Int) Bool)

    catMaybes <$> (assocs graph
        & filter (snd >>> (=='.'))
        & mapM (\((x,y),_) -> do
            c <- readArray covered (x,y)
            if c
                then pure Nothing
                else do
                    [(x,y),(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
                        & filter (inRange (bounds graph))
                        & mapM_ (flip (writeArray covered) True)
                    pure (Just (x,y))
        ))

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = splitAt k xs
    & second (chunksOf k)
    & uncurry (:)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
