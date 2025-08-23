{-# LANGUAGE MultiWayIf #-}

import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [p,h,t] <- getLine <&> (words >>> map read)
    let (q,r) = (t-p) `quotRem` p
    print $ if
        | t < p+h   -> 0
        | p <= h    -> (t-p) `div` h
        | r >= h    -> q + 1
        | otherwise -> q
