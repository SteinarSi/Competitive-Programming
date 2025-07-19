{-# LANGUAGE MultiWayIf #-}

import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [r,g,b,k] <- getLine <&> (words >>> map read)

    print $ if
        | k == 0           -> r
        | g == 0 && b == 0 -> max r (r-1 + k-1)
        | otherwise        -> r + k
