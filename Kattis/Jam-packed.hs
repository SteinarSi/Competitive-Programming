{-# LANGUAGE MultiWayIf #-}

import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [n,k] <- getLine <&> (words >>> map read)
    let (q,r) = quotRem n k
    print $ if
        | n <= k    -> n
        | r == 0    -> k
        | otherwise -> n `div` (q+1)
