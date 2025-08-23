{-# LANGUAGE MultiWayIf #-}

main :: IO ()
main = do
    [d, a, b, h] <- fmap (map read . words) getContents
    let circle = pi * (d/2)**2
        trapizza = ((a+b) / 2) * h
    putStrLn $ if
        | abs (trapizza - circle) <= 0.000001 -> "Jafn storar!"
        | circle < trapizza                   -> "Trapizza!"
        | otherwise                           -> "Mahjong!"
