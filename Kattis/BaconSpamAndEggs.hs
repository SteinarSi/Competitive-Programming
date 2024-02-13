{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

import           Control.Arrow         ((>>>))
import           Control.Monad         (forM_, replicateM, when)
import qualified Data.ByteString.Char8 as C
import           Data.List             (sort)
import qualified Data.Map              as M
import           Data.Maybe            (fromJust)
import qualified Data.Set              as S

main :: IO ()
main = do
    n <- fmap readInt C.getLine
    when (n /= 0) $ do
        replicateM n C.getLine >>= (
                    concatMap (C.words >>> (\(x:xs) -> map (x,) xs))
                >>> foldr (\(a,b) -> M.insertWith S.union b (S.singleton a)) M.empty
                >>> M.assocs
                >>> sort
                >>> map (fmap S.toAscList)
                >>> mapM_ (\(x,xs) -> C.putStrLn (C.unwords (x:xs)))
            )
        C.putStrLn ""
        main

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
