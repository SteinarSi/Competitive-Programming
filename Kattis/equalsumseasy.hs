{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Data.Array.Unboxed    (UArray, listArray, (!))
import           Data.Bits             (shiftL, (.&.))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import qualified Data.IntMap.Strict    as M
import           Data.List             (find)
import           Data.Maybe            (fromJust, mapMaybe)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> zip [1..]
        >>> mapM_ (fmap (C.words >>> map readInt) >>> solve)
    )

solve :: (Int, [Int]) -> IO ()
solve (c,n:xs) = do
    C.putStrLn ("Case #" <> C.pack (show c) <> ": ")
    C.putStrLn $ case look M.empty 1 of
        Nothing    -> "Impossible"
        Just (a,b) -> [numbers a, numbers b]
            & map (map (show >>> C.pack) >>> C.unwords)
            & C.unlines
            & C.init
    where
        look :: M.IntMap [Int] -> Int -> Maybe (Int,Int)
        look seen mask | mask >= shiftL 1 n = Nothing
                       | otherwise = case M.lookup summ seen of
                                         Nothing -> look (M.insert summ [mask] seen) (succ mask)
                                         Just ms -> case find ((.&.mask) >>> (==0)) ms of
                                             Nothing -> look (M.adjust (mask:) summ seen) (succ mask)
                                             Just  m -> Just (mask, m)
            where
                summ = sum (numbers mask)

        arr :: UArray Int Int
        arr = listArray (0,n-1) xs

        numbers :: Int -> [Int]
        numbers mask = mapMaybe (\i -> bool Nothing (Just (arr ! i)) (mask .&. shiftL 1 i /= 0)) [0..n-1]

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
