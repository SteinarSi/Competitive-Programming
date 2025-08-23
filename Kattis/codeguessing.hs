{-# LANGUAGE MultiWayIf #-}

import Control.Arrow ((>>>))
import Data.Functor ((<&>))
import Data.List (sort)

main :: IO ()
main = do
    [p,q] <- getLine <&> (words >>> map read >>> sort)
    abba <- getLine

    let ans = if | q == p + 3       && abba == "ABBA" -> [p+1,p+2]
                 | p == 3           && abba == "BBAA" -> [1,2]
                 | q == 4           && abba == "BABA" -> [1,3]
                 | p == 6           && abba == "ABAB" -> [7,9]
                 | q == 7           && abba == "AABB" -> [8,9]
                 | p == 2 && q == 8 && abba == "BAAB" -> [1,9]
                 | otherwise                          -> [-1]

    putStrLn (unwords (map show ans))
