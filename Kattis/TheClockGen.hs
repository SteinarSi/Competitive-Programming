{-
This code outputs the array to hardcode the entire input set for TheClock.hs.
Because solving the opposite problem seemed easier than the actual problem.
-}

import           Control.Arrow    ((>>>))
import           Control.Monad    (forM_)
import           Control.Monad.ST (ST)
import           Data.Array       (Array, (!))
import           Data.Array.Base  (newArray, writeArray)
import           Data.Array.ST    (runSTArray)

main :: IO ()
main = print deg2time

deg2time :: Array Int (Int,Int)
deg2time = runSTArray $ do
    arr <- newArray (0,3595 `div` 5) (-1,-1)
    forM_ [(h,m) | h <- [0..11], m <- [0..59]] (\hm -> writeArray arr (convert hm `div` 5) hm)
    pure arr

convert :: (Int,Int) -> Int
convert (hours, minutes) = (round (10 * (m-h)) + 3600) `mod` 3600
    where
        h = (360 * fromIntegral hours + m) / 12
        m = 360 * fromIntegral minutes / 60
