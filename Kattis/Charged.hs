{-# LANGUAGE TupleSections #-}

import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (find)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
                C.lines
            >>> map C.words
            >>> (head >>> map readInt >>> head &&& (!!1))
                    &&&
                (tail >>> map ((init >>> map readInt >>> head &&& last)
                                  &&&
                               (last >>> C.head)))
            >>> uncurry visualize
            >>> C.putStr
        )

visualize :: (Int,Int) -> [((Int,Int), Char)] -> C.ByteString
visualize (n,m) ps = [1..n]
        & map (\y -> [1..m]
            & map ((,y) >>> charge ps)
            & C.pack)
        & C.unlines

charge :: [((Int,Int), Char)] -> (Int,Int) -> Char
charge ps (x,y) = find (fst >>> (==(x,y))) ps
        & maybe (map chargeFrom ps
                & sum
                & formatCharge
            ) snd
    where
        chargeFrom :: ((Int,Int), Char) -> Double
        chargeFrom ((px,py),f) | f == '+'  =   recip r
                               | otherwise = - recip r
            where
                r = sqrt ((fromIntegral x - fromIntegral px) ^ 2 + (fromIntegral y - fromIntegral py) ^ 2)

formatCharge :: Double -> Char
formatCharge x | x >= 0    = formatPos x
               | otherwise = formatNeg (-x)
    where
        formatPos :: Double -> Char
        formatPos x | x >= 1 / pi   = '0'
                    | x >= 1 / pi^2 = 'O'
                    | x >= 1 / pi^3 = 'o'
                    | otherwise     = '.'

        formatNeg :: Double -> Char
        formatNeg x | x >= 1 / pi   = '%'
                    | x >= 1 / pi^2 = 'X'
                    | x >= 1 / pi^3 = 'x'
                    | otherwise     = '.'

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
