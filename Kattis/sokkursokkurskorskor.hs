{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Data.Array.Unboxed    (Array, UArray, listArray, range, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))

main :: IO ()
main = do
    xs' <- C.getContents <&> (C.words >>> drop 1 >>> map (=="\240\159\167\166"))

    let n = length xs'

        xs :: UArray Int Bool
        xs = listArray (0,n-1) xs'

        dp :: Array (Int,Int,Int) Bool
        dp = listArray rng (map f (range rng))
          where
            rng = ((0,0,0),(n,2,2))
            f (i,socks,shoes)
                | i == n = socks == 2 && shoes == 2
                | xs ! i = socks < 2 && dp ! (i+1,socks+1,shoes) || socks > 1 && socks > shoes && dp ! (i+1,socks-1,shoes)
                | otherwise = shoes < 2 && shoes < socks && dp ! (i+1,socks,shoes+1) || shoes > 0 && dp ! (i+1,socks,shoes-1)

    putStrLn $ if dp ! (0,0,0)
        then "gilt"
        else "ógilt"
