{-# LANGUAGE LambdaCase #-}

import Data.Char (toLower)
import Control.Monad (replicateM)

main :: IO ()
main = do
    a <- input False
    b <- input False
    c <- input True
    d <- input True
    e <- input False
    f <- input False
    let p1 = [a,c,e]
        p2 = [b,d,f]
        s  = sum $ map (\case
                LT -> -1
                GT ->  1
                EQ ->  0
            ) $ zipWith compare p1 p2
    putStrLn $ case compare (s, sum p1) (0, sum p2) of
        GT -> "Player 1"
        LT -> "Player 2"
        EQ -> "Tie"

input :: Bool -> IO Int
input center = fmap ((\(n:xs) -> sum $ map (power center (read n) . map toLower) xs) . words) getLine

power :: Bool -> Int -> String -> Int
power _ _ "shadow" = 6
power _ _ "gale" = 5
power _ _ "ranger" = 4
power _ _ "anvil" = 7
power _ _ "vexia" = 3
power _ _ "guardian" = 8
power _ 4 "thunderheart" = 12
power _ _ "thunderheart" = 6
power _ _ "frostwhisper" = 2
power _ _ "voidclaw" = 3
power _ _ "ironwood" = 3
power True _ "zenith" = 9
power _ _ "zenith" = 4
power _ n "seraphina" = n
