{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative   (liftA2)
import           Control.Arrow         ((>>>))
import           Control.Monad         (replicateM, unless)
import qualified Data.ByteString.Char8 as C
import           Data.Char             (toUpper)
import           Data.Maybe            (fromJust)

data Dir = U | D | L | R
    deriving Read

main :: IO ()
main = do
    [w, l] <- fmap (C.words >>> map (readInt >>> pred)) C.getLine
    unless ((- 1) `elem` [w, l]) (solve w l >> main)

solve :: Int -> Int -> IO ()
solve w l = do
    m <- fmap readInt C.getLine
    (actual, thinks) <- fmap (map (C.words >>> (\(a:b:_) -> (read [toUpper (C.head a)], readInt b))) >>> simulate (0,0) (0,0)) (replicateM m C.getLine)
    C.putStrLn ("Robot thinks " <> format thinks)
    C.putStrLn ("Actually at "  <> format actual)
    C.putStrLn ""

    where
        format :: (Int,Int) -> C.ByteString
        format (x,y) = C.unwords (map (show >>> C.pack) [x,y])

        simulate :: (Int,Int) -> (Int,Int) -> [(Dir, Int)] -> ((Int,Int), (Int,Int))
        simulate (ax,ay) (tx,ty) [] = ((ax,ay), (tx,ty))
        simulate (ax,ay) (tx,ty) ((d,s):xs) = case d of
            U -> simulate (ax,min l (ay+s)) (tx,ty+s) xs
            D -> simulate (ax,max 0 (ay-s)) (tx,ty-s) xs
            L -> simulate (max 0 (ax-s),ay) (tx-s,ty) xs
            R -> simulate (min w (ax+s),ay) (tx+s,ty) xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
