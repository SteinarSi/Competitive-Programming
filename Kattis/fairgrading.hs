{-# LANGUAGE MultiWayIf #-}

import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [x,y,z] <- getLine <&> (words >>> map read)
    let score = (x + y + 2*z) `div` 4
    putStrLn $ if
        | score >= 90 -> "A"
        | score >= 80 -> "B"
        | score >= 70 -> "C"
        | score >= 60 -> "D"
        | otherwise   -> "F"
