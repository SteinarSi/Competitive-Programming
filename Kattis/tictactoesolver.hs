{-# LANGUAGE MultiWayIf #-}

import           Data.Functor ((<&>))
import           Data.List    (transpose)

main :: IO ()
main = do
    xss <- getContents <&> lines
    let board = xss <> transpose xss <> [map (\i -> xss!!i!!i) [0..2], map (\i -> xss!!i!!(2-i)) [0..2]]
    putStrLn $ if
        | "XXX" `elem` board -> "X"
        | "OOO" `elem` board -> "O"
        | otherwise          -> "N"
