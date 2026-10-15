{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((&&&), (>>>))
import           Data.Array            (listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)
import           Text.Printf           (printf)

main :: IO ()
main = do
    tp:strs:diffs:rest <- C.getContents <&> (C.lines >>> map C.words)

    let [t,p] = map readInt tp
        strengths = map readDouble strs
        difficulties = map readDouble diffs
        solved = length (filter ("X"==) (last rest))
        teams = zipWith (\s -> zip difficulties >>> (filter (snd >>> (=="X")) >>> length) &&& (filter (snd >>> (=="?")) >>> map (fst >>> (*s)))) strengths rest

    teams
        & map (chanceToBeat solved)
        & product
        & printf "%.6f\n"
    -- seq ans1 pure ()
    -- print ans1

    pure ()

chanceToBeat :: Int -> (Int,[Double]) -> Double
chanceToBeat ourPoints (theirPoints,ps)
    | theirPoints > ourPoints      = 0
    | theirPoints + n <= ourPoints = 1
    | otherwise                    = 1 - sum (map ((n,) >>> (dp!)) [ourPoints-theirPoints+1..n])
  where
    n = length ps
    dp = listArray ((0,-1),(n,n)) $ do
        (i,p) <- zip [0..] (undefined:ps)
        j <- [-1..n]
        pure $ if
            | j < 0            -> 0
            | i == 0 && j == 0 -> 1
            | i == 0           -> 0
            | otherwise        -> (1-p) * dp ! (i-1,j) + p * dp ! (i-1,j-1)

readDouble :: C.ByteString -> Double
readDouble s
    | C.head s == '-' = negate (readDouble (C.tail s))
    | C.length r1 <= 1 = fromIntegral int
    | otherwise = fromIntegral int + float
  where
    (int, r1)
        | C.head s == '.' = (0, s)
        | otherwise = fromJust (C.readInt s)
    Just (dec, r2) = C.readInt (C.tail r1)
    float
        | C.length r1 <= 1 = 0
        | otherwise = fromIntegral dec / (10^^fromIntegral (C.length r1 - C.length r2 - 1))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
