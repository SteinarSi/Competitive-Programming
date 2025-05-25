{-# LANGUAGE MultiWayIf #-}

import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [r,g,b] <- getContents <&> (words >>> map read) :: IO [Int]

    putStrLn $ if
        | r > max g b         -> "raudur"
        | g > max r b         -> "graenn"
        | b > max r g         -> "blar"
        | r == g && b < r     -> "gulur"
        | r == b && g < b     -> "fjolubleikur"
        | g == b && r < g     -> "blagraenn"
        | all (==0) [r,g,b]   -> "svartur"
        | all (==255) [r,g,b] -> "hvitur"
        | r == g && g == b    -> "grar"
        | otherwise           -> "othekkt"
