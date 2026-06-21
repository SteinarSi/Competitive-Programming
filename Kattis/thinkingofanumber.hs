{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         (second, (&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (isDigit)
import           Data.Function         ((&))
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> map (C.span (isDigit >>> not) >>> second readInt)
        >>> split
        >>> map (solve 1 Nothing 1)
        >>> unlines
        >>> putStr
    )

solve :: Int -> Maybe Int -> Int -> [(C.ByteString,Int)] -> String
solve lo Nothing _ [] = "infinite"
solve lo (Just hi) div [] = case [mi,mi+div..hi] of
    [] -> "none"
    xs -> unwords (map show xs)
  where
    r = lo `mod` div
    mi | r == 0    = lo
       | otherwise = lo + div - r
solve lo hi div (("greater than ",i):xs) = solve (max lo (i+1)) hi div xs
solve lo hi div (("less than ",i):xs)    = solve lo (Just (maybe (i-1) (min (i-1)) hi)) div xs
solve lo hi div (("divisible by ",i):xs) = solve lo hi (lcm div i) xs

split :: [(C.ByteString,Int)] -> [[(C.ByteString,Int)]]
split []          = []
split [_]         = []
split ((_,n):xs) = splitAt n xs
    & second split
    & uncurry (:)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
