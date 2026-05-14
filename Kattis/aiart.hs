{-# LANGUAGE MultiWayIf #-}

import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [w,h] <- getLine <&> (words >>> map read)

    let a = w*h / 2
        r = sqrt (a / pi)
        s = sqrt a

    putStrLn $ if
        | 2*r <= min w h -> "circle"
        | s   <= min w h -> "square"
        | otherwise      -> "blank"
