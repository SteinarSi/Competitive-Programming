{-# LANGUAGE MultiWayIf #-}

import           Control.Arrow ((>>>))
import           Data.Bits     (xor)
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [n,a,b] <- getContents <&> (words >>> map read)

    let y = xors (a-1) `xor` xors b

    putStrLn $ if | y == 0    -> "Enginn"
                  | y > n     -> "Gunnar"
                  | otherwise -> show y

xors :: Int -> Int
xors x = case x `mod` 4 of
    0 -> x
    1 -> 1
    2 -> x+1
    3 -> 0
