{-# LANGUAGE LambdaCase #-}

import           Control.Arrow         (second, (&&&), (>>>))
import           Data.Bits             ((.|.))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)
import           Numeric               (readInt)

main :: IO ()
main = do
    hwn:rest <- C.getContents <&> C.lines

    let [h,w,n] = map (C.readInt >>> fromJust >>> fst) (C.words hwn)
        (nums,display) = rest
            & map (C.unpack >>> readInt 2 (const True) (\case 'x' -> 1; '.' -> 0) >>> head >>> fst)
            & chunksOf h
            & init &&& last

        fit :: [Integer] -> Bool
        fit = zipWith (\d x -> x == x .|. d) display >>> and

    putStrLn $ case filter fit nums of
        [_] -> "yes"
        _   -> "no"

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = splitAt k xs
    & second (chunksOf k)
    & uncurry (:)
