{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray, getAssocs, newArray,
                                        readArray, writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> map (C.words >>> map readInt >>> golomb)
        >>> C.unlines
        >>> C.putStr
    )

golomb :: [Int] -> C.ByteString
golomb xs = runST $ do
        seen <- newArray (0,maximum xs) False
        rs <- ruler seen xs
        ass <- getAssocs seen
        let missing = [C.pack (show y) | (y,False) <- ass]
        pure $ case (rs,missing) of
            (False,_)     -> "not a ruler"
            (True,["0"])  -> "perfect"
            (True,"0":ys) -> C.unwords ("missing" : ys)
            (True,ys)     -> "not a ruler"
    where
        ruler :: forall s. STUArray s Int Bool -> [Int] -> ST s Bool
        ruler seen []     = pure True
        ruler seen (x:xs) = do
            rs <- mapM (subtract x >>> abs >>> \y -> readArray seen y >>= \r -> writeArray seen y True >> pure r) xs
            if or rs
                then pure False
                else ruler seen xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
