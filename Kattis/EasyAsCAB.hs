{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict              #-}
{-# LANGUAGE TupleSections       #-}

import           Control.Applicative   (liftA3)
import           Control.Arrow         ((>>>))
import           Control.Monad         (filterM, join, replicateM, unless)
import           Control.Monad.ST      (ST, runST)
import           Data.Array            (Array, bounds, (!))
import           Data.Array.MArray     (Ix, MArray, freeze, getAssocs,
                                        getBounds, newArray, readArray,
                                        writeArray)
import           Data.Array.ST         (STArray, STUArray)
import           Data.Bool             (bool)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)
import           Data.Word             (Word8)

word :: Char -> Word8
word = pure >>> C.pack >>> B.unpack >>> head

a :: Word8
a = word 'a'

main :: IO ()
main = do
    [d', n] <- fmap C.words C.getLine
    let d = head (B.unpack d')
    xs <- fmap (C.lines >>> map B.unpack >>> trim) C.getContents
    C.putStrLn $ runST $ do
        inout <- join $ liftA3 (analyze xs) (newArray (a, d) 0) (newArray (a, d) []) (newArray ((a,a), (d,d)) False)
        case inout of
            Nothing -> pure "IMPOSSIBLE"
            Just (indeg, outdeg) -> do
                start <- getAssocs indeg <&> (filter (snd >>> (0==)) >>> map fst)
                cycle <- hasCycle outdeg
                if cycle
                    then pure "IMPOSSIBLE"
                    else topSort [] indeg outdeg start

hasCycle :: Array Word8 [Word8] -> ST s Bool
hasCycle outdeg = do
    seen <- newArray (bounds outdeg) False
    stack <- newArray (bounds outdeg) False
    lazierOr (search seen stack) [a..snd (bounds outdeg)]

    where
        search :: STUArray s Word8 Bool -> STUArray s Word8 Bool -> Word8 -> ST s Bool
        search seen stack c = do
            s <- readArray seen c
            d <- readArray stack c
            if s
                then pure d
                else do
                    writeArray seen c True
                    writeArray stack c True
                    ret <- lazierOr (search seen stack) (outdeg ! c)
                    writeArray stack c False
                    pure ret

topSort :: [Word8] -> STUArray s Word8 Int -> Array Word8 [Word8] -> [Word8] -> ST s C.ByteString
topSort ret indeg _ [] = pure (B.pack (reverse ret))
topSort _ _ _ (_:_:_)  = pure "AMBIGUOUS"
topSort ret indeg outdeg [u] = filterM (\v -> modifyArray indeg v pred >> (0==) <$> readArray indeg v) (outdeg ! u) >>= topSort (u:ret) indeg outdeg

analyze :: forall s. [[Word8]] -> STUArray s Word8 Int -> STArray s Word8 [Word8] -> STUArray s (Word8,Word8) Bool -> ST s (Maybe (STUArray s Word8 Int, Array Word8 [Word8]))
analyze [] indeg outdeg _ = freeze outdeg <&> ((indeg,) >>> Just)
analyze (x:xs) indeg outdeg seen = mapM (process x) xs >>= bool (pure Nothing) (analyze xs indeg outdeg seen) . and
    where
        process :: [Word8] -> [Word8] -> ST s Bool
        process [] _ = pure True
        process _ [] = pure False
        process (x:xs) (y:ys) | x == y = process xs ys
                              | otherwise = readArray seen (x,y) >>= flip unless (writeArray seen (x,y) True >> modifyArray outdeg x (y:) >> modifyArray indeg y succ) >> pure True

modifyArray :: (MArray a b m, Ix i) => a i b -> i -> (b -> b) -> m ()
modifyArray arr ix f = readArray arr ix >>= writeArray arr ix . f

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst

lazierOr :: (a -> ST s Bool) -> [a] -> ST s Bool
lazierOr _ []     = pure False
lazierOr f (x:xs) = f x >>= bool (lazierOr f xs) (pure True)

trim :: [[Word8]] -> [[Word8]]
trim ((x:xs):ys) | all ((x ==) . head) ys = trim (xs : map tail ys)
                 | otherwise = (x:xs):ys
