{-# LANGUAGE MultiWayIf #-}

import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

yes, no, may :: String
yes = "YES"
no = "NO"
may = "MAYBE"

main :: IO ()
main = do
    [k, n] <- getLine <&> (words >>> map read)
    putStrLn $ if
        | n == 0                         -> no
        | n == 1                         -> yes
        | k == 2                         -> yes
        | n == 2                         -> no
        | odd n                          -> may
        | even n && (n `div` 2) >= (k-1) -> may
        | even n                         -> no
        | otherwise                      -> error "bruh"
