{-# LANGUAGE Strict #-}

import Data.Maybe (fromJust)
import Control.Applicative (liftA2)
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = liftA2 (solve 0 0) (fmap r C.getLine) (fmap (map ((\[a, b] -> (r a, r b)) . C.words) . tail . C.lines) C.getContents) >>= print
    where r = fst . fromJust . C.readInt

solve :: Int -> Int -> Int -> [(Int, Int)] -> Int
solve ret par w [] = ret
solve ret par w ((a, b):xs) = solve (ret + n) r w xs
    where (n, r) = quotRem (par + a*b) w
