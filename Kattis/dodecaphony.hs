{-# LANGUAGE MultiWayIf #-}

import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))
import           Data.List     (elemIndex)
import           Data.Maybe    (fromJust)

main :: IO ()
main = do
    getLine
    xs <- getLine <&> words
    ys <- getLine <&> words

    putStrLn $ if | transposition (xs,ys) -> "Transposition"
                  | retrograde (xs,ys)    -> "Retrograde"
                  | inversion (xs,ys)     -> "Inversion"
                  | otherwise             -> "Nonsense"

transposition :: ([String], [String]) -> Bool
transposition (xs,ys) = any ((\i -> map (toInt >>> (+i) >>> fromInt) xs) >>> (==ys)) [0..length order]

retrograde :: ([String], [String]) -> Bool
retrograde (xs,ys) = reverse xs == ys

inversion :: ([String], [String]) -> Bool
inversion (x:xs,ys) = ys == x : map (toInt >>> (\i -> toInt x - (i - toInt x)) >>> fromInt) xs

toInt :: String -> Int
toInt = (`elemIndex` order) >>> fromJust

fromInt :: Int -> String
fromInt = (`mod` length order) >>> (order!!)

order :: [String]
order = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]
