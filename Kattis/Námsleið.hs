{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((&&&), (>>>))
import           Control.Monad         (filterM, forM, forM_, (>=>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array            (Array, (!))
import           Data.Array.ST         (MArray, STArray, STUArray, freeze,
                                        getAssocs, newArray, newListArray,
                                        readArray, writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Ix               (Ix)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n':pres' <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))
    let n = head n'
        ps = map tail pres'
        ans = runST $ do
            indeg <- newListArray (1,n) (map head pres')
            out   <- newArray (1,n) [] :: ST s (STArray s Int [Int])
            map tail pres'
                & zip [1..]
                & mapM_ (\(u, p) -> forM_ p $ \v -> modifyArray out v (u:))
            out' <- freeze out
            s <- getAssocs indeg <&> (filter (snd >>> (==0)) >>> map fst)
            solve (n-length s) indeg out' s
    C.putStr (format ans)

format :: Maybe [[Int]] -> C.ByteString
format Nothing    = "Omogulegt!\n"
format (Just xss) = xss
        & map ((\xs -> length xs : xs) >>> map (show >>> C.pack) >>> C.unwords)
        & (C.pack (show (length xss)) :)
        & ("Mogulegt!" :)
        & C.unlines

solve :: Int -> STUArray s Int Int -> Array Int [Int] -> [Int] -> ST s (Maybe [[Int]])
solve 0 _ _ s       = pure (Just [s])
solve _ _ _ []      = pure Nothing
solve n indeg out s = do
    s' <- forM s ((out!) >>> filterM (\v -> modifyArray indeg v pred <&> (==0))) <&> concat
    fmap (fmap (s:)) (solve (n-length s') indeg out s')

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst

modifyArray :: (MArray a b m, Ix i) => a i b -> i -> (b -> b) -> m b
modifyArray arr ix f = readArray arr ix >>= (f >>> writeArray arr ix &&& pure >>> uncurry (>>))
