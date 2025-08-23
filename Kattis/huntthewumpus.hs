{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Control.Monad         (unless, when)
import qualified Data.ByteString.Char8 as C
import           Data.Function         (on)
import           Data.Functor          ((<&>))
import           Data.List             (nub)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    seed <- C.getLine <&> readInt
    guesses <- C.getContents <&> (C.lines >>> map readInt)
    let wumpies = take 4 . nub . map (`mod`100) . tail $ iterate rng seed
    simulate 0 wumpies guesses

simulate :: Int -> [Int] -> [Int] -> IO ()
simulate score _ [] = C.putStrLn $ "Your score is " <> C.pack (show score) <> " moves."
simulate score ws (x:xs) = do
    when (length ws /= length ws') $ C.putStrLn "You hit a wumpus!"
    unless (null ws') $ C.putStrLn . C.pack . show  . minimum $ map manhattan ws'
    simulate (score+1) ws' xs

    where manhattan w = abs (x `div` 10 - w `div` 10) + abs (x `mod` 10 - w `mod` 10)
          ws' = filter (manhattan >>> (>0)) ws

rng :: Int -> Int
rng s = s + s `div` 13 + 15

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
