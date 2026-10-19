{-# LANGUAGE LambdaCase #-}

import           Control.Arrow ((>>>))
import           Control.Monad (guard)
import           Data.Char     (isAlpha)
import           Data.Functor  ((<&>))
import           Data.List     (groupBy)

main :: IO ()
main = do
    [x,y,z] <- getLine <&> (groupBy (\x y -> isAlpha x == isAlpha y) >>> filter (any isAlpha))

    let plausible = do
            a <- ['0'..'9']
            b <- ['0'..'9']
            guard (a /= b)
            c <- ['0'..'9']
            guard (a /= c)
            guard (b /= c)
            d <- ['0'..'9']
            guard (a /= d)
            guard (b /= d)
            guard (c /= d)
            e <- ['0'..'9']
            guard (a /= e)
            guard (b /= e)
            guard (c /= e)
            guard (d /= e)
            guard (parse (a,b,c,d,e) x + parse (a,b,c,d,e) y == parse (a,b,c,d,e) z)
            pure [a,b,c,d,e]

    putStrLn $ case plausible of
        xs:_ -> xs
        []   -> "impossible"

parse :: (Char,Char,Char,Char,Char) -> String -> Integer
parse (a,b,c,d,e) xs = read (map (\case
    'a' -> a
    'b' -> b
    'c' -> c
    'd' -> d
    'e' -> e
    ) xs)
