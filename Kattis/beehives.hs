{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Control.Monad         (replicateM, when)
import           Data.Array            (listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))

main :: IO ()
main = do
    [d, n'] <- C.getLine <&> (C.words >>> map readDouble)
    let n = round n'
    when (n /= 0) $ do
        points <- replicateM n C.getLine <&> (map (C.words >>> map readDouble >>> (\(a:b:_) -> (a,b))) >>> listArray (1,n))
        let sour = length $ filter (\i -> any (\j -> i /= j && sqDist (points ! i) (points ! j) <= d*d) [1..n]) [1..n]
        C.putStrLn $ C.unwords [C.pack (show sour), "sour,", C.pack (show (n-sour)), "sweet"]
        main

sqDist :: (Double, Double) -> (Double, Double) -> Double
sqDist (x,y) (a,b) = abs (x-a)**2 + abs (y-b)**2

readDouble :: C.ByteString -> Double
readDouble = C.unpack >>> read
