{-# LANGUAGE MultiWayIf #-}

import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [x,y] <- getLine <&> (words >>> map read)
    putStrLn $ if
        | y /= 1    -> show (x / (1-y))
        | x == 0    -> "ALL GOOD"
        | otherwise -> "IMPOSSIBLE"
