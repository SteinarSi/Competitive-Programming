{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Control.Monad         (replicateM, replicateM_)
import qualified Data.ByteString.Char8 as C
import           Data.List             (find, sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n <- fmap readInt C.getLine
    replicateM_ n $ do
        [s, h] <- fmap (C.words >>> map readInt) C.getLine
        hatches <- fmap (map (C.words >>> map readInt >>> (\(a:b:_)->(a,b))) >>> sort >>> reverse) (replicateM h C.getLine)
        let spots = [(x,y) | x <- [0..s], y <- [0..s], (x,y) `notElem` hatches]
        putStrLn $ case find (eligible s hatches) spots of
            Nothing    -> "poodle"
            Just (x,y) -> show x ++ " " ++ show y

eligible :: Int -> [(Int,Int)] -> (Int,Int) -> Bool
eligible s hs (x,y) = all (sqdist (x,y) >>> (<=d)) hs
    where d = fromIntegral (minimum [ x, y, s-x, s-y ] ^ 2)

sqdist :: (Int,Int) -> (Int,Int) -> Double
sqdist (a,b) (c,d) = abs (aa-cc) ** 2 + abs (bb-dd) ** 2
    where [aa,bb,cc,dd] = map fromIntegral [a,b,c,d]

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
