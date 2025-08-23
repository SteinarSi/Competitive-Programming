import           Control.Arrow         ((>>>))
import           Control.Monad         (forM_, join, when)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (UArray, freeze, newArray, newArray_,
                                        readArray, writeArray, (!))
import           Data.Array.ST         (STUArray)
import qualified Data.ByteString.Char8 as C
import           Data.Char             (digitToInt, isDigit, isUpper, ord)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> parse
        >>> map (solve >>> show)
        >>> unlines
        >>> putStr
    )

solve :: C.ByteString -> Int
solve xs = runST $ do
    (bot,cnt) <- join (precompute <$> newArray_ (0, 61) <*> newArray (0,61) 0)
    seen <- newArray (0,61) False
    solulu bot cnt seen 0 (n-1) (n-1)
  where
    n = C.length xs

    solulu :: UArray Int Int -> UArray Int Int -> STUArray s Int Bool -> Int -> Int -> Int -> ST s Int
    solulu bot cnt seen ret longest i
        | i < 0 = pure (ret * 5)
        | otherwise = do
            let c = hash (C.index xs i)
            s <- readArray seen c
            if s
                then solulu bot cnt seen ret longest (i-1)
                else do
                    writeArray seen c True
                    solulu bot cnt seen (ret + cnt ! c * (bot ! c - longest)) (longest - cnt ! c) (i-1)

    precompute :: STUArray s Int Int -> STUArray s Int Int -> ST s (UArray Int Int,UArray Int Int)
    precompute bottle count = do
        forM_ [n-1,n-2 .. 0] $ \i -> do
            let h = hash (C.index xs i)
            prev <- readArray bottle h
            c <- readArray count h
            when (c == 0) (writeArray bottle h i)
            writeArray count h (c+1)
        bot <- freeze bottle
        cnt <- freeze count
        pure (bot,cnt)

parse :: [C.ByteString] -> [C.ByteString]
parse []         = []
parse (_:xs:xss) = xs : parse xss
parse [_]        = error "bruh"

hash :: Char -> Int
hash x | isDigit x = digitToInt x
       | isUpper x = ord x - ord 'A' + 10
       | otherwise = ord x - ord 'a' + 36
