{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (C.lines
        >>> tail
        >>> map (C.words >>> map readInt >>> (\(a:b:_) -> (a,b)))
        >>> foldr ((=<<) . intersect) (Just (0,1000))
        >>> maybe "edward is right" (const "gunilla has a point")
        >>> C.putStrLn
    )

intersect :: (Int,Int) -> (Int,Int) -> Maybe (Int,Int)
intersect (a,b) (x,y) | b < x = Nothing
                      | y < a = Nothing
                      | x <= a && a <= y = Just (a,y)
                      | x <= b && b <= y = Just (x,b)
                      | a <= x && y <= b = Just (x,y)
                      | x <= a && b <= y = Just (a,b)
                      | otherwise = error "bruh"

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
