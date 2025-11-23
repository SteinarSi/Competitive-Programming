{-# LANGUAGE MultiWayIf #-}

import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [r,c] <- getLine <&> (words >>> map read)

    print $ if
        | r >= 3 && c <= 1 -> 1
        | r >= 3           -> -1
        | r == 1           -> (c+2) `div` 3
        | otherwise        -> (c+2) `div` 2
