{-# LANGUAGE MultiWayIf #-}

import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [r,d] <- getContents <&> (words >>> map read)

    print $ if
        | r == 1 && d == 1 -> 0
        | r == 1           -> (d-1) * 2
        | d == 1           -> (r-1) * 2
        | odd r && odd d   -> r * d + 1
        | otherwise        -> r * d
