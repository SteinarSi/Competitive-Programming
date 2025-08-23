{-# LANGUAGE MultiWayIf #-}

import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))
import           Data.List     (transpose)

main :: IO ()
main = do
    rows@[a,b,c] <- getContents <&> (lines >>> map (filter (`elem` "XO_")))
    let cols = transpose rows
        diag = [[a!!0,b!!1,c!!2], [a!!2,b!!1,c!!0]]
        alll = rows ++ cols ++ diag
    putStrLn $ if
        | elem "XXX" alll -> "Johan har vunnit"
        | elem "OOO" alll -> "Abdullah har vunnit"
        | otherwise       -> "ingen har vunnit"
