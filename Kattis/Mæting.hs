{-# LANGUAGE TupleSections #-}

import           Control.Monad (replicateM)
import           Data.List     (sortBy)
import           Data.Map      (Map, adjust, assocs, fromList, insert)
import           Data.Ord      (Down (Down), comparing)
import           Data.Tuple    (swap)

main :: IO ()
main = do
    n <- getLine
    people <- fmap (fromList . map (,0)) (replicateM (read n) getLine)
    attendancies <- fmap (concatMap (tail . words) . tail . lines) getContents

    let counts = foldr (adjust succ) people attendancies
        sorted = (sortBy (comparing Down) . map swap) $ assocs counts

    mapM_ (\(attendance, name) -> putStrLn (show attendance ++ " " ++ name)) sorted
